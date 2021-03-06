(* #require "lwt"
  #require "lambdasoup"
  #require "cohttp"
  #require "cohttp-lwt-unix" *)
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Soup
open Book
open Book.Lol
open Notty
module Term = Notty_lwt.Term

(* querying, soupifying *)
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

(* result display screen Notty images *)
let selected_books sels books = List.map (fun sel -> List.nth books sel) sels
    
let book_images books cur sels : image list =
  let selected_books = selected_books sels books in 
  let needs_color book = List.mem book selected_books in
  List.map (fun book -> image_of_book ~color:(needs_color book) ~cur:(List.nth books cur == book) book) books
    
let book_overview books cur sels =
  match books with
  | [] -> I.string A.empty "No results, try another search term."
  | _ ->
     List.fold_left I.(<->) I.empty (book_images books cur sels)

(* main loop *)
let rec main_loop ?download:(download = false) t books cur sels: unit Lwt.t =
  let open Lwt.Syntax in
  (* let selections = if List.length books > List.fold_left max 0 sels then
   *                    List.map (fun sel -> List.nth books sel) sels
   *                  else [] in *)
  if download then
    download_loop t books cur sels
  else
    (* ordinary display and input loop *)
    let* () = Term.image t (book_overview books cur sels) in
    let* event = Lwt_stream.get ( Term.events t) in
    match event with
    | None -> main_loop t books cur sels
    | Some (`Resize _ | #Unescape.event as x) ->
       match x with
       | `Key (`ASCII 'q',_) | `Key (`ASCII 'Q',_) -> Term.release t >>= fun () ->
                                                      Lwt.return_unit
       | `Key (`ASCII 'd',_) | `Key (`ASCII 'D',_) -> main_loop
                                                        t
                                                        books
                                                        cur
                                                        (match sels with | [] -> [cur] | _ -> sels)
                                                        ~download:true
       | `Key (`ASCII 's',_) ->
          let query = "search: " in
          search_loop t query
       | `Key (`ASCII 'm',_) | `Key (`ASCII ' ', _) ->
          let newsels =
            if List.mem cur sels then
              List.filter (fun x -> x != cur) sels
            else (cur :: sels) in
          main_loop t books cur newsels
       | `Key (`Arrow `Up, _) | `Key (`ASCII 'p',_) -> main_loop t books (max 0 (cur - 1)) sels
       | `Key (`Arrow `Down, _) | `Key (`ASCII 'n',_) -> main_loop t books (min (List.length books - 1) (cur + 1)) sels
       | _ -> Lwt.return () >>= fun () ->
              main_loop t books cur sels 

and download_loop t books cur (sels: int list) =
  let open Lwt.Syntax in
      (* downloading routine *)
    let downloading_img selection =
      I.string A.(fg white)
        (Printf.sprintf
          "Attempting %s%s.%s"
           selection.author
           selection.year
           selection.extension) in
    
    let progress_img selection =
      I.string A.(fg lightgreen)
        (Printf.sprintf
           "Downloading %s%s.%s"
           selection.author
           selection.year
           selection.extension) in

    let done_img selection =
      I.string A.(fg green)
        (Printf.sprintf
           "Finished %s%s.%s"
           selection.author
           selection.year
           selection.extension) in

    let failed_img selection e =
      I.string A.(fg red)
        (Printf.sprintf
           "Failed to download %s%s.%s: %s"
           selection.author
           selection.year
           selection.extension
           e) in

    let selected_books = selected_books sels books in
    let download_imgs = Array.of_list
                          (List.map
                             downloading_img
                             selected_books) in
    
    let disp download_imgs = (List.fold_left I.(<->) I.empty
                                (Array.to_list download_imgs)) in

    
    let rec allow_quit () =
      let* event = Lwt_stream.get ( Term.events t) in
      match event with
      | None -> allow_quit ()
      | Some (`Resize _ | #Unescape.event as x) ->
         match x with
         | `Key (`ASCII 'q',_) | `Key (`ASCII 'Q',_) -> let new_sels = (if sels == [cur] then [] else sels) in
                                                        Lwt.return new_sels
         | _ -> allow_quit ()
    in

    let* () = Term.image t (disp download_imgs) in
    let download_books =
      let* dl_links = dl_links selected_books in
      let img_mtx = Lwt_mutex.create () in
      let* () = Lwt_list.iter_p
                  begin fun (book,i) ->
                  let* res =
                    match List.nth dl_links i with
                    | Some dl_link ->
                       Ok (dl_link) |> Lwt.return
                    | None ->
                       Error "No download link available." |> Lwt.return
                  in
                  match res with
                  | Ok dl_link ->
                     let* () = Lwt_mutex.lock img_mtx in
                     download_imgs.(i) <- progress_img book;
                     let* () = Term.image t (disp download_imgs) in
                     Lwt_mutex.unlock img_mtx;
                     begin
                       let* do_dl = download_book book dl_link in
                       match do_dl with
                       | Ok _ ->
                          let* () = Lwt_mutex.lock img_mtx in
                          download_imgs.(i) <- done_img book;
                          let* () = Term.image t (disp download_imgs) in
                          Lwt_mutex.unlock img_mtx;
                          Lwt.return_unit
                       | Error e ->
                          let* () = Lwt_mutex.lock img_mtx in
                          download_imgs.(i) <- failed_img book e;
                          let* () = Term.image t (disp download_imgs) in
                          Lwt_mutex.unlock img_mtx;
                          Lwt.return_unit
                     end
                  | Error e ->
                     let* () = Lwt_mutex.lock img_mtx in
                     download_imgs.(i) <- failed_img book e;
                     let* () = Term.image t (disp download_imgs) in
                     Lwt_mutex.unlock img_mtx;
                     Lwt.return_unit
                  end
                  (Base.List.zip_exn selected_books (Base.List.range 0 (List.length selected_books))) in
      let* _ = allow_quit () in (* gives overview of downloads, fails, etc. before user exits the downloading screen *)
      Lwt.return [] in
    (* n.b., Lwt.pick will cancel all the outstanding download promises when we quit *)
    let* new_sels = Lwt.pick [allow_quit (); download_books] in
    main_loop t books cur new_sels

and search_loop t query =
  let open Lwt.Syntax in
  
  let rec allow_quit () =
    let* event = Lwt_stream.get ( Term.events t) in
    match event with
    | None -> allow_quit ()
    | Some (`Resize _ | #Unescape.event as x) ->
       match x with
       | `Key (`Enter, _) ->
          let* books =
            let search_term =
              query |> fun s -> Base.String.drop_prefix s (String.length "search: ")
            in
            let* soup = libgen_soup search_term in
            soup |> function
                   | Some soup -> let books = books_of_soup soup in
                                  Lwt.return books
                   | None -> Lwt.return []
          in
          main_loop t books 0 []
       | `Key (`ASCII 'c', [`Ctrl]) | `Key (`ASCII 'd', [`Ctrl]) ->
          let* () = Term.release t in
          Lwt.return_unit
       | `Key (`ASCII u, _) ->
          search_loop t (query ^ (String.make 1 u))
       | `Key (`Uchar u, _) ->
          search_loop t (query ^ (String.make 1 (Uchar.to_char u)))
       | `Key (`Backspace, _) ->
          if String.length query > String.length "search: " then
            search_loop t (Base.String.drop_suffix query 1)
          else
            allow_quit ()
       | _ -> search_loop t query
  in

  let grid xxs = xxs |> List.map I.hcat |> I.vcat in

  let outline attr w h =
    let chr x = I.uchar attr x 1 1
    and hbar  = I.uchar attr (Uchar.of_int 0x2500) (w - 2) 1
    and vbar  = I.uchar attr (Uchar.of_int 0x2502) 1 (h) in
    let (a, b, c, d) = (chr (Uchar.of_int 0x256d), chr (Uchar.of_int 0x256e), chr (Uchar.of_int 0x256f), chr (Uchar.of_int 0x2570)) in
    grid [ [a; hbar; b]; [vbar; I.void (w - 2) 1; vbar]; [d; hbar; c] ] in

  let (w, h) = Term.size t in
  let smack_dab i = i |> I.vsnap ~align:`Middle h
                      |> I.hsnap ~align:`Middle w in
  let img = I.((string A.empty query |> smack_dab) </>
               (outline A.(fg lightgreen) (String.length query + 2) 1 |> smack_dab)) in
  let* () = Term.image t img in
  allow_quit ()

let () =

  let t = Term.create () in
  let open Lwt.Syntax in 
  Lwt_main.run begin
      let* books =
        let search_term =
          if Array.length Sys.argv <= 1 then ""
          else Sys.argv.(1)
        in
        let* soup = libgen_soup search_term in
        soup |> function
               | Some soup -> let books = books_of_soup soup in
                              Lwt.return books
               | None -> Lwt.return []
      in
      main_loop t books 0 []
    end

