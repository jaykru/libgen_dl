(* #require "lwt"
  #require "lambdasoup"
  #require "cohttp"
  #require "cohttp-lwt-unix" *)
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Soup
open Book

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
  let open Lwt.Syntax in
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

  let rec loop ?download:(download = false) t books cur sels: unit Lwt.t =
    let open Lwt.Syntax in
    let selections = if List.length books > List.fold_left max 0 sels then
                       List.map (fun sel -> List.nth books sel) sels
                     else [] in

    (* downloading routine *)
    let downloading_img selection =
      I.string A.(fg white)
        (Printf.sprintf
           "Downloading %s.%s"
           selection.title
           selection.extension) in

    let done_img selection =
      I.string A.(fg green)
        (Printf.sprintf
           "Finished %s.%s"
           selection.title
           selection.extension) in

    let failed_img selection e =
      I.string A.(fg red)
        (Printf.sprintf
           "Failed to download %s.%s: %s"
           selection.title
           selection.extension
           e) in
    
    let download_imgs = Array.of_list
                          (List.map
                             downloading_img
                             selections) in
    
    let img download_imgs = if download then
                (List.fold_left I.(<->) I.empty
                   (Array.to_list download_imgs))
              else book_overview books cur selections in

    Term.image t (img download_imgs) >>=
      if download then
        fun _ ->
        (* TODO: make (multiple) downloads more responsive/parallel *)
        let download_books =
          let* () = Lwt_list.iter_p
                      (fun (book,i) ->
                        let* res = download_book book in
                        let new_img = match res with
                          | Ok _ -> done_img book
                          | Error e -> Printf.printf "%s\n" e;
                                       failed_img book e
                        in
                        download_imgs.(i) <- new_img;
                        Lwt
                        Term.image t (img download_imgs)
                      )
                      (Base.List.zip_exn selections (Base.List.range 0 (List.length selections))) in
          Lwt.return [] in
        
        let rec allow_quit () =
          let* event = Lwt_stream.get ( Term.events t) in
          match event with
          | None -> allow_quit ()
          | Some (`Resize _ | #Unescape.event as x) ->
             match x with
             | `Key (`ASCII 'q',_) | `Key (`ASCII 'Q',_) -> Lwt.return sels
             | _ -> allow_quit ()
        in
        (* n.b., Lwt.pick will cancel all the outstanding download promises when we quit *)
        let* new_sels = Lwt.pick [allow_quit (); download_books] in
        loop t books cur new_sels

      else
        (* ordinary display and input loop *)
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
           | `Key (`ASCII 'm',_) | `Key (`ASCII ' ', _) ->
              let newsels =
                if List.mem cur sels then
                  List.filter (fun x -> x != cur) sels
                else (cur :: sels) in
              loop t books cur newsels
           | `Key (`Arrow `Up, _) | `Key (`ASCII 'p',_) -> loop t books (max 0 (cur - 1)) sels
           | `Key (`Arrow `Down, _) | `Key (`ASCII 'n',_) -> loop t books (min (List.length books - 1) (cur + 1)) sels


           | _ -> Lwt.return () >>= fun () ->
                  loop t books cur sels in

  let t = Term.create () in
  let open Lwt.Syntax in 
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
