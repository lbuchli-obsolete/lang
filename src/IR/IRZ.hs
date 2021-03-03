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
        (Store (CTag "#C#Int" [Lit "1"]))
        (SVal $ Var "t1")
        (Seq
          (Store (CTag "#C#Int" [Lit "10"]))
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
        (CTag "#C#Int" [Var "m'"])
        (Seq
          (Ap "eval" [Var "n"])
          (CTag "#C#Int" [Var "n'"])
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
                      (Store (CTag "#C#Int" [Var "m''"]))
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
          [ (CPatCTag "#C#Int" ["x'"], SExpr $ Unit (SVal $ Var "v"))
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
        , ( CPatCTag "countTo" ["v3_pat"]
          , Seq
            (Ap "countTo" [Var "v3_pat"])
            (SVal (Var "countTo_res"))
            (Seq (Update "x" (SVal (Var "countTo_res")))
                 Empty
                 (SExpr (Unit (SVal (Var "countTo_res"))))
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
        , ( CPatCTag "error" []
          , Seq
            (Ap "error" [])
            (SVal (Var "error_res"))
            (Seq (Update "x" (SVal (Var "error_res")))
                 Empty
                 (SExpr (Unit (SVal (Var "error_res"))))
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
              (Expr
                (Seq
                  (Expr
                    (Seq (Expr (SExpr (Store (SVal (Lit "0")))))
                         (SVal (Var "v0"))
                         (SExpr (Ap "countTo" [Var "v0"]))
                    )
                  )
                  (SVal (Var "evalpatv1"))
                  (SExpr (Ap "#eval" [Var "evalpatv1"]))
                )
              )
              (SVal (Var "v1"))
              (SExpr (Store (CTag "intPrint" [Var "v1"])))
            )
          )
          (SVal (Var "v2"))
          (SExpr (Unit (SVal (Var "v2"))))
        )
      )
      (SVal (Var "main_toeval"))
      (SExpr (Ap "#eval" [Var "main_toeval"]))
    )
  , Binding
    "countTo"
    ["v3"]
    (Seq
      (Ap "#eval" [Var "v3"])
      (SVal (Var "v3_eval"))
      (Seq
        (Expr
          (Seq
            (Expr
              (Seq
                (Expr
                  (Seq
                    (Expr (SExpr (Store (SVal (Var "v3_eval")))))
                    (SVal (Var "v4"))
                    (Seq (Expr (SExpr (Store (SVal (Lit "10")))))
                         (SVal (Var "v5"))
                         (SExpr (Ap "intGT" [Var "v4", Var "v5"]))
                    )
                  )
                )
                (SVal (Var "evalpatv6"))
                (SExpr (Ap "#eval" [Var "evalpatv6"]))
              )
            )
            (SVal (Var "v6"))
            (Case
              (SVal (Var "v6"))
              [ (CPatTag "CTrue", SExpr (Store (SVal (Var "v3_eval"))))
              , ( CPatTag "CFalse"
                , Seq
                  (Expr
                    (Seq
                      (Expr
                        (Seq
                          (Expr (SExpr (Store (SVal (Var "v3_eval")))))
                          (SVal (Var "v7"))
                          (Seq (Expr (SExpr (Store (SVal (Lit "1")))))
                               (SVal (Var "v8"))
                               (SExpr (Ap "intAdd" [Var "v7", Var "v8"]))
                          )
                        )
                      )
                      (SVal (Var "evalpatv9"))
                      (SExpr (Ap "#eval" [Var "evalpatv9"]))
                    )
                  )
                  (SVal (Var "v9"))
                  (SExpr (Store (CTag "countTo" [Var "v9"])))
                )
              ]
            )
          )
        )
        (SVal (Var "v10"))
        (SExpr (Unit (SVal (Var "v10"))))
      )
    )
  ]
