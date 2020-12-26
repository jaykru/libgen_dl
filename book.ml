(** * book.ml: basic types and functions for dealing with abstract books

"And further, by these, my son, be admonished: of making many books there is no end; and much study is a weariness of the flesh."
Ecclesiastes 12:12, KJV.

 *)

open Soup
open Lwt
open Cohttp
open Cohttp_lwt_unix

(* Keeping all these members as strings will hopefully make the
   scraping a little more robust in the face of libgen changing their
   data representation *)
type book =  
  {
    (* id: string; *)
    lol_link: string; (* library.lol download /page/ link *)
    title: string;
    author: string;
    filesize: string;
    extension: string;
    (* md5: string; *)
    year: string;
    language: string;
    pages: string;
    publisher: string;
    (* edition: string;
     * cover_url: string;
     * download_url: string;
     * page_url: string; *)
  }

let take_nodes start (p: 'a node -> bool) (next: 'a node -> 'a node option): 'a node list =
  let rec aux cur acc  =
    if not (p cur) then
      match next cur with
      | None -> acc
      | Some sib -> aux (coerce sib) (cur::acc)
    else
      (cur :: acc) (* we want to include the node that breaks p *)
  in aux (coerce start) [] |> List.rev

(* let take_until start p = take_nodes start p next_sibling *)
let take_back_until start p = take_nodes start p previous_sibling

let is_final (n: 'a node) : bool =
  match previous_element n with
  | None -> false
  | Some esib ->
     match element n with
     | Some en ->
        begin match (attribute "nowrap" en, attribute "nowrap" esib) with
        | Some(""), Some("") -> true
        | _ -> false end
     | _ -> false

let is_initial (n: 'a node): bool =
  match element n with
  | Some en ->
     begin match attribute "href" en with
     | None -> false
     | Some a -> Re.execp (Re.Perl.compile_pat "library.lol") a
     end
  | None -> false

let book_nodes_as_text (book: 'a node list): string list list =
  book
  |> List.map coerce
  |> List.filter is_element
  |> List.map texts

(** Cleans up malformed UTF-8 *)
let cleaned ?encoding:(encoding = Some `UTF_8) (src : [`Channel of in_channel | `String of string]) =
  let rec loop d buf acc = match Uutf.decode d with
    | `Uchar u ->
       begin match Uchar.to_int u with
       | 0xa0 -> ignore ()
       (* TODO: Better solution for this bug. I think this is a problem in Uutf. *)
       | _ -> Uutf.Buffer.add_utf_8 buf u
       end;
       loop d buf acc
    | `End -> List.rev (Base.Buffer.contents buf :: acc)
    | `Malformed _ -> Uutf.Buffer.add_utf_8 buf Uutf.u_rep; loop d buf acc
    | `Await -> assert false
  in
  let res = loop (Uutf.decoder ?encoding src) (Base.Buffer.create 128) []
  |> String.concat "" in
  Printf.printf "cleaned: %s\n" res;
  Zed_utf8.validate res |> ignore;
  res

let strings_to_book ~lol_link:(lol_link: string) (as_text: string list list): book =
  (* TODO: try to filter out some of the junk libgen throws in; perhaps
   not worth doing considering we'll clip at a certain number of
   characters anyway *)
  (* This is a little bit chatty, and kind of yucky looking *)
  let title = Caml.String.concat "; " (List.nth as_text 1) in
  let publisher = String.concat " " (List.nth as_text 2) in
  let year = String.concat " " (List.nth as_text 3) in
  let pages = String.concat " " (List.nth as_text 4) in
  let language = String.concat ", " (List.nth as_text 5) in
  let filesize = String.concat " " (List.nth as_text 6) in
  let extension = String.concat ", " (List.nth as_text 7) in
  let authors = String.concat "" (List.nth as_text 0) in
  
  {
    lol_link = (cleaned (`String lol_link));
    title = (cleaned (`String title));
    author = (cleaned (`String authors));
    filesize = (cleaned (`String filesize));
    extension = (cleaned (`String extension));
    year = (cleaned (`String year));
    language = (cleaned (`String language));
    pages = (cleaned (`String pages));
    publisher = (cleaned (`String publisher));
  }

let is_cf_ipfs_gateway (n: 'a node): bool =
  match element n with
  | Some en ->
     begin match attribute "href" en with
     | None -> false
     | Some a -> Re.execp (Re.Perl.compile_pat "cloudflare-ipfs.com") a
     end
  | None -> false

let get_cf_ipfs_link (lol_link: string): string option Lwt.t =
  let lol_query = lol_link in
  let open Lwt.Syntax in
  
  let* lol =
    begin
      (* TODO: figure out how to search libgen over tls *)
      Client.get (Uri.of_string lol_query) >>= fun (_, body) ->
      (* let code = resp |> Response.status |> Code.code_of_status in *)
      body |> Cohttp_lwt.Body.to_string >|= fun body ->
      body
    end in
  let lol_soup = parse lol in
  
  let cloudflare_dl_link =
    let open Base.Option.Let_syntax in
    lol_soup
    |> descendants
    |> filter is_cf_ipfs_gateway
    |> elements
    |> first >>= fun elt ->
    elt |> attribute "href"
  in
  Lwt.return cloudflare_dl_link


let download_book (b: book) (dl_link: string) =
  let open Lwt.Syntax in

  let* file = 
       Client.get (Uri.of_string dl_link) >>= fun (resp, body) ->
       let code = resp |> Response.status |> Code.code_of_status in
       match code with
       | 200 -> 
          body |> Cohttp_lwt.Body.to_string >|= fun body ->
          Ok body
       | _ -> Lwt.return_error "Bad response code."
  in
  match file with
  | Ok file ->
     Ok (Lwt_io.with_file
           ~mode:Output (b.author ^ b.year ^ "." ^ b.extension) (fun f ->
             Lwt_io.write f file))
     |> Lwt.return
  | Error e ->
     (* TODO: if we can clean up the title we get, we can potentially
        avoid doing this *)
     Printf.sprintf "Error downloading %s%s.%s: %s" b.author b.year b.extension e |> Lwt.return_error


(* TODO: remove uses of require *)
let lol_links soup =
  soup 
  |> descendants
  |> filter is_initial
  |> to_list
  |> List.map (fun x ->
         let open Base.Option.Let_syntax in
         element x >>= fun e ->
         attribute "href" e)
  |> List.map require

(* gets the cloudflare_ipfs links for a particular set of books *)
let dl_links (bs: book list): string option list Lwt.t =
  let dl_links = List.map (fun b -> b.lol_link) bs in
  let open Lwt.Syntax in
  let m = Lwt_mutex.create () in
  if List.length bs > 1 then
    let* dl_links =
      Lwt_list.map_p
        (fun lol_link ->
          let* () = Lwt_mutex.lock m in
          let* cf_ipfs_link = get_cf_ipfs_link lol_link in
          let* () = Lwt_unix.sleep 0.05 in
          Lwt_mutex.unlock m;
          Lwt.return cf_ipfs_link)
        dl_links in
    Lwt.return dl_links
  else
    (* if we're just downloading a single book, no need to be gentle with library.lol *)
    Lwt_list.map_p get_cf_ipfs_link dl_links

let books_of_soup soup : book list =
  let lol_links = lol_links soup in
  let books_nodes = soup
                    |> descendants
                    |> filter is_final
                    |> to_list
                    |> List.map (fun final -> take_back_until final (fun _ -> false))
                    |> List.map List.rev in
  
  List.map book_nodes_as_text books_nodes
  (* TODO: I'm not sure that we can actually assume every libgen book is on
     library.lol, but this hasn't triggered an exception yet... *)
  |> Base.List.zip_exn lol_links 
  |> List.map (fun (lol_link, as_text) ->
         strings_to_book ~lol_link:lol_link as_text)

let image_of_book ?color:(color = false) ?cur:(cur = false) (b: book): Notty.image =
  let open Notty in 
  let open I in
  let open A in
  let trunc ?max_len:(max_len = 40) f =
    let trunced = Core.String.slice f 0 (min (String.length f) max_len) in
    if String.length trunced < String.length f then
      trunced ^ "..."
    else
      trunced
  in
  
  let title = string
                (if cur then
                   fg white
                 else A.empty)
                (trunc b.title)
              <|> string A.empty " | " in 
  let author = string
                 (if cur then
                    fg yellow
                  else A.empty)
                 (trunc b.author ~max_len:10)
               <|> string A.empty " | " in
  let year = string
               (if cur then
                  fg lightmagenta
                else A.empty)
               b.year
             <|> string A.empty " | " in 
  let size = string 
               (if cur then
                  fg lightgreen
                else A.empty)
               b.filesize
             <|> string A.empty " | " in
  let filetype = string
                   (if cur then
                      fg red
                    else A.empty)
                   b.extension in
  (if color then I.string A.(fg green) " * "  else I.empty) <|>
    List.fold_left (<|>) I.empty [title; author; year; size; filetype]
