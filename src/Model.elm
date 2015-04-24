module Model where


type alias Group = 
    { id            : Int
    , name          : String
    , screen_name   : String
    , is_closed     : Int
    --, type'       : String
    , photo_50      : String
    , photo_100     : String
    , photo_200     : String
    }

type alias Post =
    { id        : Int
    , text      : String
    , likes     : Int
    , date      : Float
    , date_str  : String
    , photos    : List Photo
    , audios    : List Audio
    } 

type alias Photo = 
    { id        : Int
    , owner_id  : Int
    , photo_75  : String
    , photo_130 : String
    , photo_604 : String
    , width     : Int
    , height    : Int
    }

type alias Audio =
    { id        : Int
    , owner_id  : Int
    , title     : String
    , artist    : String
    }

type alias User =
    { id        : Int
    , first_name: String
    , last_name : String
    , photo     : String
    }

type PostWindow = Window Post String | None

type GroupWindow = GroupWindow Post | NoneGroup

type FriendWindow = FriendWindow Post | NoneFriend

type alias State  = 
    { currentToggle : String
    , currentGroup  : Int
    , groups        : List Group
    , posts         : List Post
    , openPosts     : List Int
    , postWindow    : PostWindow
    , friendWindow  : FriendWindow
    , friends       : List User
    , groupWindow   : GroupWindow
    , postGroups    : List Group
    , winSize       : (Int, Int)
    }
