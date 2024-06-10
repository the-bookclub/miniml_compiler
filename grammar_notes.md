e_top    := (e_top)
          | e_zeroth
e_zeroth := e_1 (e_2)
          | e_1 e_2
          | e_first
e_first  := e_1 + e_2
          | e_second
e_second := e_1 and e_2
          | e_third
e_third  := e_1 :: e_2
          | e_fourth
e_fourth := e_1 == e_2
          | e_fifth
e_fifth  := fn x . e
          | e_null
e_null   := x
          | c_bool
          | c_num
          | let d in e_top
          | not(e_top)
          | if e_top then e_top else e_top
          | succ(e_top)
          | <e_top,e_top>
          | fst(e_top)
          | snd(e_top)
          | nil
          | hd(e_top)
          | tl(e_top)
          | pred(e_top)

x = [a-zA-Z_][a-zA-Z1-9]*
c_bool = true | false
c_num = [0-9]+
