{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module IR.IRZ where

newtype IRZ = IRZ [Binding] deriving Show
type Var = String
type Const = String
type Lit = String
data Binding = Binding Var [Var] Expr deriving Show
data Expr = Seq SExpr LPat Expr
          | Case Val [(CPat, Expr)]
          | SExpr SExpr
  deriving Show
data SExpr = Ap Var [SVal]
           | Unit Val
           | Store Val
           | Fetch Var
           | Update Var Val
           | Expr Expr
  deriving Show
data Val = CTag Const [SVal]
         | VTag Var [SVal]
         | Tag Const
         | Empty
         | SVal SVal
  deriving Show
data SVal = Lit Lit
          | Var Var
  deriving Show
type LPat = Val
data CPat = CPatCTag Const [Var]
          | CPatTag Const
          | CPatLit Lit
          | CPatVar Var
  deriving Show

example2 :: IRZ
example2 = undefined

{--
example2 :: IRZ
example2 = IRZ
  [ (Binding
      "main"
      []
      (Seq
        (Store (CTag "CInt" [Lit "1"]))
        (SVal $ Var "t1")
        (Seq
          (Store (CTag "CInt" [Lit "10"]))
          (SVal $ Var "t2")
          (Seq
            (Store (CTag "Fupto" [Var "t1", Var "t2"]))
            (SVal $ Var "t3")
            (Seq
              (Ap "eval" [Var "t3"])
              (SVal $ Var "result")
              (Seq (Ap "showHeap" [])
                   (Empty)
                   (SExpr $ Unit (SVal $ Var "result"))
              )
            )
          )
        )
      )
    )
  , (Binding
      "upto"
      ["m", "n"]
      (Seq
        (Ap "eval" [Var "m"])
        (CTag "CInt" [Var "m'"])
        (Seq
          (Ap "eval" [Var "n"])
          (CTag "CInt" [Var "n'"])
          (Seq
            (Ap "intGT" [Var "m'", Var "n'"])
            (SVal $ Var "b")
            (Case
              (SVal $ Var "b")
              [ (CPatTag "CTrue", (SExpr $ Unit (Tag "CNil")))
              , ( CPatTag "CFalse"
                , (Seq
                    (Ap "intAdd" [Var "m'", Lit "1"])
                    (SVal $ Var "m''")
                    (Seq
                      (Store (CTag "CInt" [Var "m''"]))
                      (SVal $ Var "t5")
                      (Seq (Ap "upto" [Var "t5", Var "n"])
                           (SVal $ Var "t6")
                           (SExpr $ Unit (CTag "CCons" [Var "m", Var "t6"]))
                      )
                    )
                  )
                )
              ]
            )
          )
        )
      )
    )
  , (Binding
      "eval"
      ["p"]
      (Seq
        (Fetch "p")
        (SVal $ Var "v")
        (Case
          (SVal $ Var "v")
          [ (CPatCTag "CInt" ["x'"], SExpr $ Unit (SVal $ Var "v"))
          , (CPatTag "CNil"        , SExpr $ Unit (SVal $ Var "v"))
          , ( CPatCTag "CCons" ["x", "xs"]
            , (Seq
                (Ap "eval" [Var "x"])
                (SVal $ Var "x'")
                (Seq
                  (Ap "eval" [Var "xs"])
                  (SVal $ Var "xs'")
                  (Seq (Update "p" (CTag "CCons" [Var "x'", Var "xs'"]))
                       (Empty)
                       (SExpr $ Unit (SVal $ Var "p"))
                  )
                )
              )
            )
          , ( CPatCTag "Fupto" ["a", "b"]
            , (Seq
                (Ap "upto" [Var "a", Var "b"])
                (SVal $ Var "w")
                (Seq (Update "p" (SVal $ Var "w"))
                     (Empty)
                     (SExpr $ Unit (SVal $ Var "w"))
                )
              )
            )
          ]
        )
      )
    )
  ]

--}

translated :: IRZ
translated = IRZ
  [ Binding
    "#eval"
    ["x"]
    (Seq
      (Fetch "x")
      (SVal (Var "v"))
      (Case
        (SVal (Var "v"))
        [ ( CPatCTag "main" []
          , Seq
            (Ap "main" [])
            (SVal (Var "main_res"))
            (Seq (Update "x" (SVal (Var "main_res")))
                 Empty
                 (SExpr (Unit (SVal (Var "main_res"))))
            )
          )
        , ( CPatCTag "upto" ["v3_pat", "v4_pat"]
          , Seq
            (Ap "upto" [Var "v3_pat", Var "v4_pat"])
            (SVal (Var "upto_res"))
            (Seq (Update "x" (SVal (Var "upto_res")))
                 Empty
                 (SExpr (Unit (SVal (Var "upto_res"))))
            )
          )
        , ( CPatCTag "sum" ["v8_pat"]
          , Seq
            (Ap "sum" [Var "v8_pat"])
            (SVal (Var "sum_res"))
            (Seq (Update "x" (SVal (Var "sum_res")))
                 Empty
                 (SExpr (Unit (SVal (Var "sum_res"))))
            )
          )
        , ( CPatCTag "intAdd" ["a_pat", "b_pat"]
          , Seq
            (Ap "intAdd" [Var "a_pat", Var "b_pat"])
            (SVal (Var "intAdd_res"))
            (Seq (Update "x" (SVal (Var "intAdd_res")))
                 Empty
                 (SExpr (Unit (SVal (Var "intAdd_res"))))
            )
          )
        , ( CPatCTag "intPrint" ["p_pat"]
          , Seq
            (Ap "intPrint" [Var "p_pat"])
            (SVal (Var "intPrint_res"))
            (Seq (Update "x" (SVal (Var "intPrint_res")))
                 Empty
                 (SExpr (Unit (SVal (Var "intPrint_res"))))
            )
          )
        , ( CPatCTag "intGT" ["c_pat", "d_pat"]
          , Seq
            (Ap "intGT" [Var "c_pat", Var "d_pat"])
            (SVal (Var "intGT_res"))
            (Seq (Update "x" (SVal (Var "intGT_res")))
                 Empty
                 (SExpr (Unit (SVal (Var "intGT_res"))))
            )
          )
        , ( CPatCTag "showHeap" []
          , Seq
            (Ap "showHeap" [])
            (SVal (Var "showHeap_res"))
            (Seq (Update "x" (SVal (Var "showHeap_res")))
                 Empty
                 (SExpr (Unit (SVal (Var "showHeap_res"))))
            )
          )
        , (CPatVar "_", SExpr (Unit (SVal (Var "v"))))
        ]
      )
    )
  , Binding
    "main"
    []
    (Seq
      (Expr
        (Seq
          (Expr
            (Seq
              (Store (CTag "CInt" [Lit "10"]))
              (SVal (Var "v1"))
              (Seq (Store (CTag "CInt" [Lit "1"]))
                   (SVal (Var "v0"))
                   (SExpr (Ap "upto" [Var "v0", Var "v1"]))
              )
            )
          )
          (SVal (Var "v2"))
          (SExpr (Store (CTag "sum" [Var "v2"])))
        )
      )
      (SVal (Var "main_toeval"))
      (SExpr (Ap "#eval" [Var "main_toeval"]))
    )
  , Binding
    "upto"
    ["v3", "v4"]
    (Seq
      (Ap "#eval" [Var "v4"])
      (SVal (Var "v4_eval"))
      (Seq
        (Ap "#eval" [Var "v3"])
        (SVal (Var "v3_eval"))
        (Seq
          (Expr (SExpr (Ap "intGT" [Var "v3_eval", Var "v4_eval"])))
          (SVal (Var "v5"))
          (Case
            (SVal (Var "v5"))
            [ (CPatTag "CTrue", SExpr (Unit (Tag "CNil")))
            , ( CPatTag "CFalse"
              , Seq
                (Expr (SExpr (Store (CTag "intAdd" [Var "v3_eval", Lit "1"]))))
                (SVal (Var "v6"))
                (Seq
                  (Expr (SExpr (Store (CTag "upto" [Var "v6", Var "v4_eval"]))))
                  (SVal (Var "v7"))
                  (SExpr (Unit (CTag "CCons" [Var "v6", Var "v7"])))
                )
              )
            ]
          )
        )
      )
    )
  , Binding
    "sum"
    ["v8"]
    (Seq
      (Ap "#eval" [Var "v8"])
      (SVal (Var "v8_eval"))
      (Case
        (SVal (Var "v8_eval"))
        [ (CPatTag "CNil", SExpr (Unit (CTag "CInt" [Lit "0"])))
        , ( CPatCTag "CCons" ["v9", "v10"]
          , Seq
            (Expr (SExpr (Ap "sum" [Var "v10"])))
            (CTag "CInt" [Var "v11"])
            (Seq (Expr (SExpr (Store (CTag "intAdd" [Var "v9", Var "v11"]))))
                 (SVal (Var "v12"))
                 (SExpr (Unit (SVal (Var "v12"))))
            )
          )
        ]
      )
    )
  ]
