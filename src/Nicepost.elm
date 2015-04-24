module Nicepost where

import Html (..)
import Html.Attributes as HA
import Html.Events (..)
import Html.Lazy (lazy, lazy2)

import Signal
import List
import Maybe
import Text
import String
import Graphics.Element as GE
import Graphics.Input as GI

import Utils (..)
import Model (..)
import Update (..)
import View (..)



------------- Utils funcitons ------------

dividePosts : List Post -> (List Post, List Post, List Post)
dividePosts posts = divideList [] [] [] posts 0

getNextImg : Post -> String -> String
getNextImg post imgSrc =
    getNext post.photos post.photos .photo_604 imgSrc

{------------- View ----------------------}

display : State -> GE.Element
display {currentToggle, currentGroup, groups, posts, openPosts, postWindow, friendWindow, friends, groupWindow, postGroups, winSize} =
    let imageWin = 
            case postWindow of
                Window post img -> [getPostWindow winSize post img, getBlackBackground]
                _ -> []
        friendWin = 
            case friendWindow of 
                FriendWindow post -> [getFriendsWindow winSize friends postGroups post, getBlackBackground]
                _ -> []
        groupWin = 
            case groupWindow of 
                GroupWindow post -> [getGroupWindow winSize postGroups post, getBlackBackground]
                _ -> []
    in 
        div [] ([
            lazy2 displayToggles toggleList currentToggle,
            lazy2 groupsDiv groups currentGroup,
            lazy2 postColumns posts openPosts
        ] ++ imageWin ++ friendWin ++ groupWin)
        |> toElement 930 700

getBlackBackground : Html
getBlackBackground =
    div [HA.class "win_black"] []

getPostWindow : (Int, Int) -> Post -> String -> Html
getPostWindow (windowHeight, scrollTop) post imgSrc = 
    let imgStyle = 
        if  | List.length post.photos > 1 -> 
                [ HA.style [("cursor", "pointer")]
                , HA.alt "Loading"
                , onClick (Signal.send actions (OpenImage (Window post (getNextImg post imgSrc)))) ]
            | otherwise -> []
    in 
    div [ HA.class "win_container",
            HA.style [("top", (toString scrollTop ++ "px"))]]
        [ div [HA.class "win"]
            [
                p [HA.class "post_window_p"]
                    [
                    a [HA.class "close", 
                        onClick (Signal.send actions (OpenImage None)) ]
                        [
                        text "Закрыть"
                        ]
                    ]
                , img ( [HA.class "win_img", HA.src imgSrc] ++ imgStyle ) []
            ]
        ]


getFriendsWindow : (Int, Int) -> List User -> List Group -> Post -> Html
getFriendsWindow (windowHeight, scrollTop) friends groups post =
    div [ HA.class "win_container",
            HA.style [("top", (toString scrollTop ++ "px"))]]
        [ div [HA.class "window_friend"]
            [
                p [HA.class "post_window_p"]
                    [
                    a [HA.class "close", 
                        onClick (Signal.send actions (OpenFriendWindow NoneFriend)) ]
                        [
                        text "Закрыть"
                        ]
                    ]
                , div [HA.class "friend_container"]
                    ((List.map (getGroupLine post) groups) ++
                    (List.map (getFriendLine post) friends))
            ]
        ]

getFriendLine : Post -> User -> Html
getFriendLine post user =
    span [HA.class "friend_item"
        , onClick (Signal.send actions (Repost post user.id))]
    [   img [HA.class "friend_img", HA.src user.photo] []
        , a [HA.class "friend_text"]
            [ text (user.last_name ++ " " ++ user.first_name)
            ]
    ]


getGroupWindow : (Int, Int) -> List Group -> Post -> Html
getGroupWindow (windowHeight, scrollTop) groups post = 
    div [ HA.class "win_container",
            HA.style [("top", (toString scrollTop ++ "px"))]]
        [ div [HA.class "window_friend"]
            [
                p [HA.class "post_window_p"]
                    [
                    a [HA.class "close", 
                        onClick (Signal.send actions (OpenGroupWindow NoneGroup)) ]
                        [
                        text "Закрыть"
                        ]
                    ]
                , div [HA.class "friend_container"]
                    (List.map (getGroupLine post) groups)
            ]
        ]

getGroupLine : Post -> Group -> Html
getGroupLine post group =
    span [HA.class "friend_item"
        , onClick (Signal.send actions (Repost post (-group.id)))]
    [   img [HA.class "friend_img", HA.src group.photo_50] []
        , a [HA.class "friend_text"]
            [ text (group.name)
            ]
    ]


{----------------- elm-html ---------------}

postColumns : List Post -> List Int -> Html
postColumns posts openPosts =
    case posts of 
        [] -> displayLoader
        _ ->
            let (posts1, posts2, posts3) = dividePosts posts 
            in 
                div [HA.class "columns"] 
                    [ postColumn "post-column1" posts1 openPosts
                    , postColumn "post-column2" posts2 openPosts
                    , postColumn "post-column3" posts3 openPosts
                    ]


postColumn : String -> List Post -> List Int -> Html
postColumn name posts openPosts = 
    div [HA.class "columnpost", HA.id name]
        (List.map (postHtml openPosts) posts)


postHtml : List Int -> Post -> Html
postHtml openPosts post = 
    let imgs = 
        case post.photos of
            ph :: t ->  getImgs post
            _       ->  []
    in 
        div [HA.class "main"]
            ( imgs
            ++ (getTextBlock post openPosts)
            ++ (getAudios post)
            ++ [postFooter post]
            )


getTextBlock : Post -> List Int -> List Html
getTextBlock post openPosts =
    if  | String.length post.text == 0 -> []
        | otherwise ->
            let (txt, className) =
                if  | String.length post.text > 240 ->
                    case List.filter (\id -> id == post.id) openPosts of
                        [] -> ( (String.left 140 post.text) ++ "... (развернуть)", "textBlockClicked")
                        _ -> (post.text, "textBlockClicked")
                    | otherwise -> (post.text, "textBlock")
            in 
            [div [HA.class className
                , onClick (Signal.send actions (ClickText post.id)) ]
                [
                    text txt
                ] 
            ]


getImgs : Post -> List Html
getImgs post =
    let photos = post.photos
        getImg_290 = getImg 270 .photo_604 post
        getImg_144 = getImg 134 .photo_604 post
        getImg_95  = getImg 89  .photo_130 post
    in 
        case List.length photos of
        1 -> List.map getImg_290 photos 
        2 -> List.map getImg_144 photos
        3 -> List.map getImg_95 (List.take 3 photos)
        4 -> List.map getImg_144 (List.take 2 photos) 
             ++ List.map getImg_144 (last2_3 photos)
        5 -> List.map getImg_144 (List.take 2 photos)  
             ++ List.map getImg_95 (last2_3 photos) 
        _ -> List.map getImg_95 (List.take 6 photos)


last2_3 : List a -> List a
last2_3 (_ :: _ :: t) = t

getImg : Int -> (Photo -> String) -> Post -> Photo -> Html
getImg w getSrc post photo = 
    img [ HA.class "mainimg"
        , HA.src (getSrc photo)
        , HA.width w
        , onClick (Signal.send actions (OpenImage (Window post photo.photo_604)) )
        ]
        []


getAudios : Post -> List Html
getAudios post = 
     (List.map (getAudio post) post.audios)
    {--case post.audios of
        h :: t -> 
            [div [ style [prop "padding" "0px 0px 5px 0px"]]
                (map getAudio post.audios)]
        otherwise -> [] --}

getAudio : Post -> Audio -> Html
getAudio post audio =
    a [HA.class "song"
      , onClick (Signal.send actions (Repost post 0))] 
    [
        text (audio.artist ++ " - " ++ audio.title)
        --text (String.left 40 (audio.artist ++ " - " ++ audio.title))
    ]


postFooter : Post -> Html
postFooter post = 
    div [HA.class "footer"] [
        span [HA.class "date"] 
            [text post.date_str],

        div [HA.class "postbutton"
            , onClick (Signal.send actions (Repost post 0))]
            [text "На стену"],
        div [HA.class "postbutton"
            ,onClick (Signal.send actions (OpenFriendWindow (FriendWindow post)  ))]
            [text "Друзьям"],
        {-- a [HA.class "postbutton"
            ,onClick (Signal.send actions (OpenGroupWindow (GroupWindow post)  ))]
            [text "В группу"], --}
        span [HA.class "likes"] 
                [text (toString post.likes)]
    ]


displayLoader : Html
displayLoader =
    img [ HA.src "resources/loader.gif"
        , HA.style [("marginLeft", "455px"), ("marginTop", "200px")] 
        ][]


{---------------- groups -----------}

groupsDiv : List Group -> Int -> Html
groupsDiv groups currentGroup = 
    div [HA.class "groups"]
        (List.map (groupImg currentGroup) groups)


groupImg : Int -> Group -> Html
groupImg currentGroup group = 
    let cls =
        if  | group.id == currentGroup -> "group_checked"
            | otherwise ->  "groupimg"
    in
    img [
        HA.class cls,
        HA.src group.photo_50,
        HA.title (group.name ++ " | " ++ group.screen_name),
        onClick (Signal.send actions (ChangeGroup group.id) )
    ] []




{------------ toggle buttons ------}

toggleList: List String
toggleList = ["Юмор","Любовь", "Новости", "Картинки", "Умное", "Цитаты",
        "Музыка", "Игры", "Спорт", "Авто", "Бизнес",
        "Мода", "Рецепты", "English", "Открытки", "Стихи", "Рукоделие", "Мои" ]

displayToggles : List String -> String -> Html
displayToggles toggles theme = 
    --div [HA.class "toggle-btn-grp cssonly main-toggle"]
    div [HA.class "main-toggle"]
        (List.map (getToggle theme) toggles)

getToggle : String -> String -> Html
getToggle theme name = 
    let className = 
        if  | name == theme -> "toggle-btn toggle-btn-checked"
            | otherwise -> "toggle-btn toggle-btn-normal"
    in
        div [HA.class className
            , onClick (Signal.send actions (ChangeToggle name))]
        [
                text name
        ]




{-- Inputs---------}

main : Signal GE.Element
main = Signal.map display currentState

currentState : Signal State
currentState = Signal.foldp update defaultState mainSignal

defaultState : State
defaultState = 
    { currentToggle = "Юмор"
    , currentGroup  = 0
    , groups        = []
    , posts         = []
    , openPosts     = []
    , postWindow    = None -- PostWindow {showWindow = False, None, "hello"}
    , friendWindow  = NoneFriend
    , friends       = []
    , groupWindow   = NoneGroup
    , postGroups    = []
    , winSize       = (1000, 0)
    }

mainSignal : Signal Action
mainSignal = Signal.mergeMany
    [ Signal.subscribe actions
    , newGroups
    , newPosts
    , newResize
    , newPostGrpous
    , newFriends
    ]


actions : Signal.Channel Action
actions = Signal.channel Noop


-- Incoming messages --

port getGroups : Signal (List Group)
newGroups : Signal Action
newGroups = Signal.map NewGroups getGroups

port getWallPosts : Signal (Int, List Post)
newPosts : Signal Action
newPosts = 
    let convert (offset, posts) = GetPosts posts
    in Signal.map convert getWallPosts

port resize: Signal (Int, Int)
newResize : Signal Action
newResize = Signal.map Resize resize

port getPostGroups : Signal (List Group)
newPostGrpous : Signal Action
newPostGrpous = 
    let convert groups = SetGroups groups
    in Signal.map convert getPostGroups

port getPostFriends : Signal (List User)
newFriends : Signal Action
newFriends = 
    let convert friends = SetFriends friends
    in Signal.map convert getPostFriends

-- Outgoing signals --

port htmlGroupClick : Signal Int
port htmlGroupClick = 
    let pridicate act = 
        case act of
            ChangeGroup groupID -> True
            _ -> False
        select (ChangeGroup groupID) = groupID
    in Signal.map select (Signal.keepIf pridicate (ChangeGroup 0) (Signal.subscribe actions))

port htmlToggleClick : Signal String
port htmlToggleClick = 
    let predicate act =
        case act of
            ChangeToggle name -> True
            _ -> False
        select (ChangeToggle name) = name
    in Signal.map select (Signal.keepIf predicate (ChangeToggle "Юмор") (Signal.subscribe actions))

port repostClick : Signal (Post, Int) 
port repostClick =
    let predicate act = 
        case act of
            Repost post owner_id -> True
            _ -> False
        select (Repost post owner_id) = (post, owner_id) 
    in Signal.map select (Signal.keepIf predicate (Repost emptyPost 0) (Signal.subscribe actions))

emptyPost : Post 
emptyPost =   
    { id        = 0
    , text      = ""
    , likes     = 0
    , date      = 0
    , date_str  = ""
    , photos    = []
    , audios    = []
    } 