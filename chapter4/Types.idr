module Types

data Direction = North
                | East
                | South
                | West

turnClockwise : Direction -> Direction
turnClockwise North = East
turnClockwise East = South
turnClockwise South = West
turnClockwise West = North


||| Represents shapes
data Shape = ||| A triangle, with its base length and height
              Triangle Double Double
            |  ||| A Rectangle, with its length and height
              Rectangle Double Double
            | ||| A circle, with its radius
              Circle Double

%name Shape shape, shape1, shape2

area : Shape -> Double
area (Triangle base height) = base * height * 0.5
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

data Picture = Primitive Shape
              | Combine Picture Picture
              | Rotate Double Picture
              | Translate Double Double Picture

%name Picture pic, pic1, pic2

rectangle : Picture
rectangle = Primitive (Rectangle 20 10)
circle : Picture
circle = Primitive (Circle 5)
triangle : Picture
triangle = Primitive (Triangle 10 10)
testPicture : Picture 
testPicture = Combine (Translate 5 5 rectangle)
              (Combine (Translate 35 5 circle)
              (Translate 15 25 triangle))


pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic pic1) = pictureArea pic + pictureArea pic1
pictureArea (Rotate x pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic

data Biggest = NoTriangle | Size Double

biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive triangle@(Triangle x y)) = Just (area triangle)
biggestTriangle (Combine pic pic1) = case (biggestTriangle pic) of
                                      Nothing => biggestTriangle pic1
                                      (Just size) => case (biggestTriangle pic1) of
                                        Nothing => Just size
                                        Just size1 => Just (max size size1)
biggestTriangle (Rotate x pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic
biggestTriangle _ = Nothing

safeDivide : Double -> Double -> Maybe Double
safeDivide x y = if y == 0 then Nothing
                            else Just (x / y)


-- Binary Trees
data BSTree : Type -> Type where
      Empty : Ord elem => BSTree elem
      Node : Ord elem => (left: BSTree elem) -> (val : elem) -> (right : BSTree elem) -> BSTree elem

%name BSTree tree, tree1

insert : elem -> BSTree elem -> BSTree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) = case compare x val of
                                      LT => Node (insert x left) val right
                                      EQ => orig
                                      GT => Node left val (insert x right)

-- and it worked on the first try without any type errors lets goooo
listToTree : Ord elem => List elem -> BSTree elem
listToTree xs = listToTreeInner xs Empty
  where
    listToTreeInner : List elem -> BSTree elem -> BSTree elem
    listToTreeInner [] tree = tree
    listToTreeInner (x :: xs) tree = insert x (listToTreeInner xs tree)


treeToList : BSTree elem -> List elem
treeToList Empty = []
treeToList (Node left val right) = treeToList left ++ val :: treeToList right


-- expression
data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr

evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add x y) = (evaluate x) + (evaluate y)
evaluate (Sub x y) = (evaluate x) - (evaluate y)
evaluate (Mult x y) = (evaluate x) * (evaluate y)

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing y = y
maxMaybe x Nothing = x
maxMaybe a@(Just x) b@(Just y) = if x > y then a else b
