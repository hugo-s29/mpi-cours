type var = char
type lit = var * bool
type cls = lit list
type fnc = cls list

let rec parse_f (s: string) (start: int) (len: int): fnc =
  if String.length s = 0 then []
  else parse_d s start len
and parse_d (s: string) (start: int) (len: int): fnc =
  let i = ref start in
  while s.[!i] != '&' && !i < len do incr i; done;
  if !i = len then [parse_c s start len]
  else (parse_c s start (!i - 1)) :: (parse_d s (!i+1) len)
and parse_c (s: string) (start: int) (len: int): cls =
  let i = ref start in
  while s.[!i] != '|' && !i < len do incr i; done;
  if !i = len then [parse_l s start len]
  else (parse_l s start (!i - 1)) :: (parse_c s (!i+1) len)
and parse_l (s: string) (start: int) (len: int): lit =
  if s.[start] = '-' then (parse_v s (start + 1) len, false)
  else (parse_v s start len, true)
and parse_v (s: string) (start: int) (len: int): char =
  let x = s.[start] in
  assert(Char.code x >= 97 && Char.code x <= 122);
  assert(len - start = 1);
  x


