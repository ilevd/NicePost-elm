module Utils where

import List( (::) , head )

-- divide 1 list to 3
divideList : List a -> List a -> List a -> List a -> Int -> (List a, List a, List a)
divideList posts1 posts2 posts3 posts acc = 
    case posts of
        [] -> (posts1, posts2, posts3)
        h :: t -> 
            let (p1, p2, p3) = divideList posts1 posts2 posts3 t (acc+1) in
            case acc % 3 of
                0 -> ((h :: p1), p2, p3)
                1 -> (p1, (h :: p2), p3) 
                2 -> (p1, p2, (h :: p3))     


-- get next element after given ex: [1,2,3,4] 4 -> 1
getNext : List a  -> List a -> (a -> b) -> b -> b
getNext all arr f elem = 
  case arr of
    h :: t -> 
      if  | f h == elem -> f <| head (t ++ all)
          | otherwise -> getNext all t f elem         