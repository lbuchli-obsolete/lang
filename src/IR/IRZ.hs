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
        [ ( CPatCTag "testCase_" ["v0_pat"]
          , Seq
            (Ap "testCase_" [Var "v0_pat"])
            (SVal (Var "testCase__res"))
            (Seq (Update "x" (SVal (Var "testCase__res")))
                 Empty
                 (SExpr (Ap "#eval" [Var "testCase__res"]))
            )
          )
        , ( CPatCTag "main" []
          , Seq
            (Ap "main" [])
            (SVal (Var "main_res"))
            (Seq (Update "x" (SVal (Var "main_res")))
                 Empty
                 (SExpr (Ap "#eval" [Var "main_res"]))
            )
          )
        , ( CPatCTag "intAdd__" ["a_pat", "b_pat"]
          , Seq
            (Ap "intAdd__" [Var "a_pat", Var "b_pat"])
            (SVal (Var "intAdd___res"))
            (Seq (Update "x" (SVal (Var "intAdd___res")))
                 Empty
                 (SExpr (Ap "#eval" [Var "intAdd___res"]))
            )
          )
        , ( CPatCTag "intPrint_" ["p_pat"]
          , Seq
            (Ap "intPrint_" [Var "p_pat"])
            (SVal (Var "intPrint__res"))
            (Seq (Update "x" (SVal (Var "intPrint__res")))
                 Empty
                 (SExpr (Ap "#eval" [Var "intPrint__res"]))
            )
          )
        , ( CPatCTag "intGT__" ["c_pat", "d_pat"]
          , Seq
            (Ap "intGT__" [Var "c_pat", Var "d_pat"])
            (SVal (Var "intGT___res"))
            (Seq (Update "x" (SVal (Var "intGT___res")))
                 Empty
                 (SExpr (Ap "#eval" [Var "intGT___res"]))
            )
          )
        , ( CPatCTag "strConcat__" ["e_pat", "f_pat"]
          , Seq
            (Ap "strConcat__" [Var "e_pat", Var "f_pat"])
            (SVal (Var "strConcat___res"))
            (Seq (Update "x" (SVal (Var "strConcat___res")))
                 Empty
                 (SExpr (Ap "#eval" [Var "strConcat___res"]))
            )
          )
        , ( CPatCTag "strLength_" ["g_pat"]
          , Seq
            (Ap "strLength_" [Var "g_pat"])
            (SVal (Var "strLength__res"))
            (Seq (Update "x" (SVal (Var "strLength__res")))
                 Empty
                 (SExpr (Ap "#eval" [Var "strLength__res"]))
            )
          )
        , ( CPatCTag "strTake__" ["h_pat", "i_pat"]
          , Seq
            (Ap "strTake__" [Var "h_pat", Var "i_pat"])
            (SVal (Var "strTake___res"))
            (Seq (Update "x" (SVal (Var "strTake___res")))
                 Empty
                 (SExpr (Ap "#eval" [Var "strTake___res"]))
            )
          )
        , ( CPatCTag "strDrop__" ["j_pat", "k_pat"]
          , Seq
            (Ap "strDrop__" [Var "j_pat", Var "k_pat"])
            (SVal (Var "strDrop___res"))
            (Seq (Update "x" (SVal (Var "strDrop___res")))
                 Empty
                 (SExpr (Ap "#eval" [Var "strDrop___res"]))
            )
          )
        , ( CPatCTag "showHeap" []
          , Seq
            (Ap "showHeap" [])
            (SVal (Var "showHeap_res"))
            (Seq (Update "x" (SVal (Var "showHeap_res")))
                 Empty
                 (SExpr (Ap "#eval" [Var "showHeap_res"]))
            )
          )
        , ( CPatCTag "error" []
          , Seq
            (Ap "error" [])
            (SVal (Var "error_res"))
            (Seq (Update "x" (SVal (Var "error_res")))
                 Empty
                 (SExpr (Ap "#eval" [Var "error_res"]))
            )
          )
        , (CPatVar "_", SExpr (Unit (SVal (Var "v"))))
        ]
      )
    )
  , Binding
    "testCase_"
    ["v0"]
    (Seq
      (Ap "#eval" [Var "v0"])
      (SVal (Var "v0_eval"))
      (Seq
        (Expr
          (Seq
            (Expr
              (Seq (Expr (SExpr (Store (SVal (Var "v0_eval")))))
                   (SVal (Var "evalpatv1"))
                   (SExpr (Ap "#eval" [Var "evalpatv1"]))
              )
            )
            (SVal (Var "v1"))
            (Case
              (SVal (Var "v1"))
              [ ( CPatCTag "#C#Str" ["v2"]
                , Seq
                  (Ap "#eval" [Var "v2"])
                  (SVal (Var "v2_eval"))
                  (Case
                    (SVal (Var "v2"))
                    [ ( CPatLit "Ja"
                      , Seq
                        (Expr
                          (Seq (Expr (SExpr (Store (SVal (Lit "True")))))
                               (SVal (Var "evalpatv3"))
                               (SExpr (Ap "#eval" [Var "evalpatv3"]))
                          )
                        )
                        (SVal (Var "v3"))
                        (SExpr (Store (CTag "#C#Str" [Var "v3"])))
                      )
                    ]
                  )
                )
              , ( CPatCTag "#C#Str" ["v4"]
                , Seq
                  (Ap "#eval" [Var "v4"])
                  (SVal (Var "v4_eval"))
                  (Case
                    (SVal (Var "v4"))
                    [ ( CPatLit "Nein"
                      , Seq
                        (Expr
                          (Seq (Expr (SExpr (Store (SVal (Lit "False")))))
                               (SVal (Var "evalpatv5"))
                               (SExpr (Ap "#eval" [Var "evalpatv5"]))
                          )
                        )
                        (SVal (Var "v5"))
                        (SExpr (Store (CTag "#C#Str" [Var "v5"])))
                      )
                    ]
                  )
                )
              ]
            )
          )
        )
        (SVal (Var "v6"))
        (SExpr (Unit (SVal (Var "v6"))))
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
                    (Seq (Expr (SExpr (Store (SVal (Lit "Nein")))))
                         (SVal (Var "v7"))
                         (SExpr (Store (CTag "#C#Str" [Var "v7"])))
                    )
                  )
                  (SVal (Var "evalpatv8"))
                  (SExpr (Ap "#eval" [Var "evalpatv8"]))
                )
              )
              (SVal (Var "v8"))
              (SExpr (Store (CTag "testCase_" [Var "v8"])))
            )
          )
          (SVal (Var "v9"))
          (SExpr (Unit (SVal (Var "v9"))))
        )
      )
      (SVal (Var "main_toeval"))
      (SExpr (Ap "#eval" [Var "main_toeval"]))
    )
  ]
