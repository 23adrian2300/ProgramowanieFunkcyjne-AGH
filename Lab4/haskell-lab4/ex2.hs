type X = Int
type Y = Int

data CartInt2DVec = MkCartInt2DVec X Y 

xCoord :: CartInt2DVec -> Int
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Int
yCoord (MkCartInt2DVec _ y) = y

data Cart2DVec' a = MkCart2DVec' a a

xCoord' :: Cart2DVec' a -> a
xCoord' (MkCart2DVec' x _) = x

yCoord' :: Cart2DVec' a -> a
yCoord' (MkCart2DVec' _ y) = y

data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}

data List a = EmptyL | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyL      = error "head': the empty list has no head!"
head' (Cons x xs) = x


data ThreeColors = Blue |
                   White |
                   Red

type ActorName = String

leadingActor :: ThreeColors -> ActorName
leadingActor Blue  = "Juliette Binoche"
leadingActor White = "Zbigniew Zamachowski"
leadingActor Red   = "Irene Jacob"


data Cart3DVec a = Cart3DVec a a a

xCoord'' :: Cart3DVec a -> a
xCoord'' (Cart3DVec x _ _) = x

yCoord'' :: Cart3DVec a -> a
yCoord'' (Cart3DVec _ y _) = y

zCoord'' :: Cart3DVec a -> a
zCoord'' (Cart3DVec _ _ z) = z

data Cart3DVec' a = Cart3DVec' {x'::a, y'::a, z'::a}

data TrafficLights = Green | Yellow | Reed

actionFor :: TrafficLights -> String
actionFor Green = "Go"
actionFor Yellow = "Prepare to stop"
actionFor Reed = "Stop"