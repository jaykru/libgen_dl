(* #require "lwt"
  #require "lambdasoup"
  #require "cohttp"
  #require "cohttp-lwt-unix" *)
open Lwt
open Lwt.Syntax
open Cohttp
open Cohttp_lwt_unix
open Soup

(** * books
      TODO: modularize *)
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
  let* lol =
    begin
      (* TODO: figure out how to search libgen over tls *)
      Client.get (Uri.of_string query) >>= fun (_, body) ->
      (* let code = resp |> Response.status |> Code.code_of_status in *)
      body |> Cohttp_lwt.Body.to_string >|= fun body ->
      body
    end in
  let lol_soup = parse lol in
  
  
  let cloudflare_dl_link = lol_soup
                           |> descendants
                           |> filter is_cf_ipfs_gateway
                           |> elements
                           |> first
                           |> require
                           |> attribute "href"
                           |> require in
  let* file = 
    begin
      Client.get (Uri.of_string cloudflare_dl_link) >>= fun (_, body) ->
      (* let code = resp |> Response.status |> Code.code_of_status in *) (* TODO: error check *)
      body |> Cohttp_lwt.Body.to_string >|= fun body ->
      body
    end
  in
  (* TODO: if we can clean up the title we get, we can potentially
     avoid doing this *)
  Lwt_io.with_file ~mode:Output (b.author ^ b.year ^ "." ^ b.extension) (fun f ->
      Lwt_io.write f file)


(* TODO: remove uses of require *)
let lol_links soup =
  soup 
  |> descendants
  |> filter is_initial
  |> to_list
  |> List.map (fun x ->
         element x
         |> require
         |> attribute "href")
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

(** * nasty stuff *)
let query_url ?num_results:(results = 25) search_term =
  let res =
    match results with
      25 | 50 | 75 -> results
      | _ -> Base.Int.round ~dir:`Nearest ~to_multiple_of:25 results
             |> min 75
             |> max 25
  in
  Printf.sprintf
    "http://libgen.rs/search.php?req=%s&lg_topic=libgen&open=0&view=simple&res=%d&phrase=1&column=def"
    search_term
    res

let libgen_soup ?num_results:(res = 25) search_term =
  let query = query_url search_term ~num_results:res in 
  (* TODO: figure out how to search libgen over tls *)
  let bodycode =
    let* (resp, body) = Client.get (Uri.of_string query) in
    let code = resp |> Response.status |> Code.code_of_status in
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    (body, code) in 

  let* (body, code) = bodycode in
  match code with
  | 200 ->
     let soup = parse body
     in Some soup |> Lwt.return
  | _ -> None |> Lwt.return

let () =
  let open Notty in
  
  let book_images books cur sels : image list =
    let needs_color book = List.mem book sels in
    List.map (fun book -> image_of_book ~color:(needs_color book) ~cur:(List.nth books cur == book) book) books in
  
  let book_overview books cur sels =
    match books with
    | [] -> I.string A.empty "No results, try another search term."
    | _ ->
       List.fold_left I.(<->) I.empty (book_images books cur sels) in

  let module Term = Notty_lwt.Term in

  let rec loop ?download:(download = false) t books cur sels: unit Lwt.t=
    let selections = if List.length books > List.fold_left max 0 sels then
                       List.map (fun sel -> List.nth books sel) sels
                     else [] in
    
    let downloading_img selection =
      I.string A.(fg red)
        (Printf.sprintf
           "Downloading %s.%s"
           selection.title
           selection.extension) in
    
    let img = if download then
                (* I.vsnap ~align:`Bottom 1 @@ *)
                (List.fold_left I.(<->) I.empty
                   (List.map
                      downloading_img
                      selections))
              else book_overview books cur selections in
    Term.image t img >>=
      if download then
        fun _ ->
        (* TODO: make (multiple) downloads more responsive *)
        Lwt_list.iter_s download_book selections >>= fun () ->
        loop t books cur []
      else 
        fun _ ->
        let* event = Lwt_stream.get ( Term.events t) in
        match event with
        | None -> loop t books cur sels
        | Some (`Resize _ | #Unescape.event as x) ->
           match x with
           | `Key (`ASCII 'q',_) | `Key (`ASCII 'Q',_) -> Term.release t >>= fun () ->
                                                          Lwt.return_unit
           | `Key (`ASCII 'd',_) | `Key (`ASCII 'D',_) -> loop t books cur sels ~download:true
           | `Key (`ASCII 's',_) ->
              let* search_term = Lwt_io.read_line Lwt_io.stdin in
              let* new_books = libgen_soup search_term
                               >|= function
                               | Some soup -> books_of_soup soup
                               | None -> []
              in
              loop t new_books cur sels
           | `Key (`ASCII 'm',_) -> let newsels =
                                      if List.mem cur sels then
                                        List.filter (fun x -> x != cur) sels
                                      else (cur :: sels) in
                                    loop t books cur newsels

           | `Key (`Arrow `Up, _) -> loop t books (max 0 (cur - 1)) sels
           | `Key (`Arrow `Down, _) -> loop t books (min (List.length books - 1) (cur + 1)) sels


           | _ -> Lwt.return () >>= fun () ->
                  loop t books cur sels in



  let t = Term.create () in
  Lwt_main.run begin
      let* books =
        let search_term =
          if Array.length Sys.argv <= 1 then ""
          else Sys.argv.(1)
        in
        libgen_soup search_term 
        >|= function
        | Some soup -> books_of_soup soup
        | None -> []
      in
      loop t books 0 []
    end

