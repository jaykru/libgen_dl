(* #require "lwt"
 * #require "lambndasoup"
 * #require "cohttp"
 * #require "cohttp-lwt-unix" *)
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Re
open Soup

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

let take_until start p = take_nodes start p next_sibling
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

let book_as_text (book: 'a node list) = book
                                   |> List.map coerce
                                   |> List.filter is_element
                                   |> List.map texts


let strings_to_book (link: string) (as_text: string list list): book =
  (* TODO: try to filter out some of the junk libgen throws in; perhaps
   not worth doing considering we'll clip at a certain number of
   characters anyway *)
  (* let title = String.concat "; " (List.nth as_text 1) *)
  let title = List.nth (List.nth as_text 1) 1 in (* dropping information
                                           here. above definition is
                                           more informative but also
                                           chatty *)
  let publisher = String.concat " " (List.nth as_text 2) in
  let year = String.concat " " (List.nth as_text 3) in
  let pages = List.hd (List.nth as_text 4) in
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

let query =
  let search_term = "theorem proving" in
  let num_results = 25 in
  Printf.sprintf "http://libgen.rs/search.php?req=%s&lg_topic=libgen&open=0&view=simple&res=%d&phrase=1&column=def" search_term num_results

let body =
  (* TODO: figure out how to search libgen more securely *)
  Client.get (Uri.of_string query) >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Caml.Printf.printf "Response code: %d\n" code;
  Caml.Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Caml.Printf.printf "Body of length: %d\n" (String.length body);
  body

let soup = let body = Lwt_main.run body in
           parse body

let links = soup |> descendants |> filter is_initial |> to_list |> List.map (fun x -> element x |> require |> attribute "href") |> List.map require

let books_nodes = soup |> descendants |> filter is_final |> to_list |> List.map (fun final -> take_back_until final (fun _->false)) |> List.map List.rev

let books = List.map book_as_text books_nodes |> Base.List.zip_exn links |> List.map (fun (link, as_text) -> strings_to_book link as_text)
let book_links = Base.List.zip books links

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
  let lol_soup =
    begin
      (* TODO: figure out how to search libgen more securely *)
      Client.get (Uri.of_string query) >>= fun (resp, body) ->
      let code = resp |> Response.status |> Code.code_of_status in
      Caml.Printf.printf "Response code: %d\n" code;
      Caml.Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
      body |> Cohttp_lwt.Body.to_string >|= fun body ->
      Caml.Printf.printf "Body of length: %d\n" (String.length body);
      body
    end
    |> Lwt_main.run
    |> parse in
  
  let cloudflare_dl_link = lol_soup |> descendants |> filter is_cf_ipfs_gateway |> elements |> first |> require |> attribute "href" |> require in
  
  let file = 
    begin
      Printf.printf "uri: %s" cloudflare_dl_link;
      Client.get (Uri.of_string cloudflare_dl_link) >>= fun (resp, body) ->
      let code = resp |> Response.status |> Code.code_of_status in
      Caml.Printf.printf "Response code: %d\n" code;
      Caml.Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
      body |> Cohttp_lwt.Body.to_string >|= fun body ->
      Caml.Printf.printf "Body of length: %d\n" (String.length body);
      body
    end|> Lwt_main.run
  in
  Lwt_io.with_file ~mode:Output (b.title ^ "." ^ b.extension) (fun f ->
      Lwt_io.write f file)
  |> Lwt_main.run
