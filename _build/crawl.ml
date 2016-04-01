open Util ;;    
open CrawlerServices ;;
open Order ;;
open Pagerank ;;


module MoogleRanker
  = (* InDegreeRanker (PageGraph) (PageScore) *)
  
     RandomWalkRanker (PageGraph) (PageScore) (struct 
       let do_random_jumps = Some 0.20
       let num_steps = 1000
     end)
  

(* Dictionaries mapping words (strings) to sets of crawler links *)
module WordDict = Dict.Make(
  struct 
    type key = string
    type value = LinkSet.set
    let compare = string_compare
    let string_of_key = (fun s -> s)
    let string_of_value = LinkSet.string_of_set

    (* These functions are for testing purposes *)
    let gen_key () = ""
    let gen_key_gt x () = gen_key ()
    let gen_key_lt x () = gen_key ()
    let gen_key_random () = gen_key ()
    let gen_key_between x y () = None
    let gen_value () = LinkSet.empty
    let gen_pair () = (gen_key(),gen_value())
  end)

(* A query module that uses LinkSet and WordDict *)
module Q = Query.Query(
  struct
    module S = LinkSet
    module D = WordDict
  end)

let print s = 
  let _ = Printf.printf "%s\n" s in
  flush_all();;


(***********************************************************************)
(*    PART 1: CRAWLER                                                  *)
(***********************************************************************)

(* TODO: Build an index as follows:
 * 
 * Remove a link from the frontier (the set of links that have yet to
 * be visited), visit this link, add its outgoing links to the
 * frontier, and update the index so that all words on this page are
 * mapped to linksets containing this url.
 *
 * Keep crawling until we've
 * reached the maximum number of links (n) or the frontier is empty. *)
let rec crawl (n:int) (frontier: LinkSet.set)
	      (visited : LinkSet.set) (d:WordDict.dict) : WordDict.dict =
  (* helper function to insert list of links into set *)
  let rec insert_list (set: LinkSet.set) (links: link list) : LinkSet.set =
    match links with
    | hd :: tl -> let set = (LinkSet.insert hd set) in
		  insert_list set tl
    | [] -> set
  in
  (* helper function to insert words *)
  let rec insert_words (link: CrawlerServices.link) (words: string list) 
    (dict: WordDict.dict) : WordDict.dict =
    match words with
      hd :: tl -> let ls = (WordDict.lookup dict hd) in
        (match ls with
         | Some value -> 
            if (LinkSet.member value link) then insert_words link tl dict
            else let ls = (LinkSet.insert link value) in
		 let dict = (WordDict.insert dict hd ls) in
              insert_words link tl dict
         | None -> let ls = (LinkSet.singleton link) in 
		   let dict = (WordDict.insert dict hd ls) in 
		   insert_words link tl dict)
    | [] -> dict
  in
  (* helper function to get links on the page *)
  let get_links (link: CrawlerServices.link) 
    : (CrawlerServices.page * CrawlerServices.link list) option =
    let p = (CrawlerServices.get_page link) in
    match p with
    | Some page -> Some (page, page.links)
    | None -> None
  in
  if n <= 0 || (LinkSet.is_empty frontier) then d
  else let pair = (LinkSet.choose frontier) in
    match pair with
    | Some (link, new_frontier) -> 
          if (LinkSet.member visited link) then crawl n new_frontier visited d
          else let page_links = get_links link in
            (match page_links with
              Some (page, links) -> 
                let new_frontier = (insert_list new_frontier links) in
                let words = page.words in
                let d = (insert_words link words d) in
                let visited = (LinkSet.insert link visited) in
                  crawl (n-1) new_frontier visited d
              | None -> let visited = (LinkSet.insert link visited) in
                  crawl n frontier visited d)
    | None -> d
;;

let crawler () = 
  crawl num_pages_to_search (LinkSet.singleton initial_link) LinkSet.empty
    WordDict.empty
;;

(* Debugging note: if you set debug=true in moogle.ml, it will print out your
 * index after crawling. *)
