(** *

book.mli: basic types and functions for dealing with abstract books

"And further, by these, my son, be admonished: of making many books
   there is no end; and much study is a weariness of the flesh."
- Ecclesiastes 12:12, KJV.

** *)

open Soup

(* Keeping all these members as strings will hopefully make the
     scraping a little more robust in the face of libgen changing their
     data representation *)
type book = {
    lol_link : string;
    title : string;
    author : string;
    filesize : string;
    extension : string;
    year : string;
    language : string;
    pages : string;
    publisher : string;
  }
val image_of_book : ?color:bool -> ?cur:bool -> book -> Notty.image 

module type LibgenMirror = sig
  val books_of_soup : soup node -> book list
  val dl_links : book list -> string option list Lwt.t
  val download_book : book -> string -> (unit Lwt.t, string) result Lwt.t
end

module Lol : LibgenMirror
