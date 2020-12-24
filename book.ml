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
    download_link: string;
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

let strings_to_book (link: string) (as_text: string list list): book =
  (* TODO: try to filter out some of the junk libgen throws in; perhaps
   not worth doing considering we'll clip at a certain number of
   characters anyway *)
  (* This is a little bit chatty, and kind of yucky looking *)
  let title = String.concat "; " (List.nth as_text 1) in
  let publisher = String.concat " " (List.nth as_text 2) in
  let year = String.concat " " (List.nth as_text 3) in
  let pages = String.concat " " (List.nth as_text 4) in
  let language = String.concat ", " (List.nth as_text 5) in
  let size = String.concat " " (List.nth as_text 6) in
  let filetype = String.concat ", " (List.nth as_text 7) in
  let authors = String.concat "" (List.nth as_text 0) in
  
  {
    download_link = link;
    title = title;
    author = authors;
    filesize = size;
    extension = filetype;
    year = year;
    language = language;
    pages = pages;
    publisher = publisher;
  }


let is_cf_ipfs_gateway (n: 'a node): bool =
  match element n with
  | Some en ->
     begin match attribute "href" en with
     | None -> false
     | Some a -> Re.execp (Re.Perl.compile_pat "cloudflare-ipfs.com") a
     end
  | None -> false

let download_book (b: book) =
  let query = b.download_link in
  let open Lwt.Syntax in
  
  let* lol =
    begin
      (* TODO: figure out how to search libgen over tls *)
      Client.get (Uri.of_string query) >>= fun (_, body) ->
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

  let* file = 
    match cloudflare_dl_link with
      None -> Lwt.return_error "No download link found."
    | Some link ->
       Client.get (Uri.of_string link) >>= fun (resp, body) ->
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
     Printf.sprintf "Error downloading %s.%s: %s" b.title b.extension e |> Lwt.return_error
(* TODO: if we can clean up the title we get, we can potentially
     avoid doing this *)

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

let books_of_soup soup : book list =
  let links = lol_links soup in 
  let books_nodes = soup
                    |> descendants
                    |> filter is_final
                    |> to_list
                    |> List.map (fun final -> take_back_until final (fun _ -> false))
                    |> List.map List.rev in

  List.map book_nodes_as_text books_nodes
  |> Base.List.zip_exn links
  |> List.map (fun (link, as_text) -> strings_to_book link as_text)

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