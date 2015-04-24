module Update where

import List 
import Model (..)


type Action
    = Noop
    | GetPosts (List Post)
    | ChangeToggle String
    | ChangeGroup Int
    | ClickText Int
    | OpenImage PostWindow
    | OpenFriendWindow FriendWindow
    | OpenGroupWindow GroupWindow
    | SetFriends (List User)
    | SetGroups (List Group)
    | NewGroups (List Group)
    | Resize (Int, Int)
    | Repost Post Int



update : Action -> State -> State
update action state = 
    case action of
        Noop -> state

        NewGroups groups -> 
            case groups of
                h :: t -> {state | groups <- groups, currentGroup <- h.id, openPosts <- []} --, posts <- []}
                nogroup -> {state | groups <- nogroup, openPosts <- [], posts <- []}

        ChangeGroup groupID ->
            {state | posts <- [], openPosts <- [], currentGroup <- groupID}

        GetPosts posts ->
            case state.posts of
                [] -> { state | posts <- posts}
                oldPosts -> { state | posts <- oldPosts ++ posts}

        ClickText postID ->
            case List.filter (\id -> id == postID) state.openPosts of
                [] -> { state | openPosts <- [postID] ++ state.openPosts }
                _ -> { state | openPosts <- List.filter (\id -> id /= postID) state.openPosts }

        ChangeToggle themeName ->
            { state | openPosts <- [], posts <- [], currentToggle <- themeName}

        OpenImage postWindow -> 
            { state | postWindow <- postWindow}

        Resize (windowHeight, scrollTop) ->
            { state | winSize <- (windowHeight, scrollTop) }

        Repost post owner_id ->
            state

        OpenFriendWindow friendWindow ->
            { state | friendWindow <- friendWindow }

        OpenGroupWindow groupWindow ->
            { state | groupWindow <- groupWindow }

        SetFriends friends ->
            { state | friends <- friends}

        SetGroups groups ->
            { state | postGroups <- groups}