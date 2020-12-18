(* #require "lwt"
 * #require "lambndasoup"
 * #require "cohttp"
 * #require "cohttp-lwt-unix" *)
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Re
open Soup


(* Keeping all these members are strings will hopefully make us a
   little more robust in the face of libgen changing their HTML *)
type book =  
  {
    id: string;
    title: string;
    author: string;
    filesize: string;
    extension: string;
    md5: string;
    year: string;
    language: string;
    pages: string;
    publisher: string;
    edition: string;
    cover_url: string;
    download_url: string;
    page_url: string;
  }

let query =
  let search_term = "theorem proving" in
  let num_results = 25 in
  Printf.sprintf "http://libgen.rs/search.php?req=%s&lg_topic=libgen&open=0&view=simple&res=%d&phrase=1&column=def" search_term num_results

let body =
  (* TODO: figure out how to search libgen more securely *)
  Client.get (Uri.of_string query) >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

let soup = let body = Lwt_main.run body in
           parse body

let authorp x = x |> to_string |> Re.execp (Re.Pcre.re "column=author" |> compile)

let all_tds = soup $$ "td"
let filtered_tds = all_tds |> filter authorp

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
        | Some a -> a |> Re.execp (Re.Perl.compile_pat "http://library.lol") end
     | None -> false

let Some(yves) = first filtered_tds
let res = take_until
            (coerce yves)
            is_initial
          |> List.map coerce
          |> List.filter is_element
          |> List.map texts

let authors = String.concat "" (List.nth res 0)

(* TODO: try to filter out some of the junk libgen throws in; maybe
   not worth doing considering we'll clip at a certain number of
   characters anyway *)
(* let title = String.concat "; " (List.nth res 1) *)
let title = List.nth (List.nth res 1) 1 (* dropping information
                                           here. above definition is
                                           more informative but also
                                           chatty *)
let publisher = String.concat " " (List.nth res 2)
let year = String.concat " " (List.nth res 3)
let pages = List.hd (List.nth res 4)
let language = String.concat ", " (List.nth res 5)
let size = String.concat " " (List.nth res 6)
let filetype = String.concat ", " (List.nth res 7)

(* need to get download link *)
let todo = take_back_until (coerce yves) is_initial |> List.map to_string 
