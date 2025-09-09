let limb_bits = 30
let base = 1 lsl limb_bits
let base_mask = base - 1

type t = { sign : int; limbs : int array }

let zero = { sign = 0; limbs = [||] }
let one = { sign = 1; limbs = [| 1 |] }
let minus_one = { sign = -1; limbs = [| 1 |] }
let is_zero x = x.sign = 0
let make n = Array.make n 0

let normalize { sign; limbs } =
  let n = Array.length limbs in
  let rec trim i =
    if i < 0 then -1 else if limbs.(i) = 0 then trim (i - 1) else i
  in
  let last = trim (n - 1) in
  if last = -1 then zero
  else if last = n - 1 then { sign; limbs }
  else { sign; limbs = Array.init (last + 1) (fun i -> limbs.(i)) }

let sign x = x.sign

let of_int n =
  if n = 0 then zero
  else if n > 0 then { sign = 1; limbs = [| n land base_mask |] }
  else { sign = -1; limbs = [| -n land base_mask |] }

let of_int64 n =
  if n = 0L then zero
  else
    let sign = if n < 0L then -1 else 1 in
    let absn = if n < 0L then Int64.neg n else n in
    let low = Int64.to_int (Int64.logand absn (Int64.of_int base_mask)) in
    let high = Int64.to_int (Int64.shift_right_logical absn limb_bits) in
    if high = 0 then { sign; limbs = [| low |] }
    else { sign; limbs = [| low; high |] }

let to_int x =
  if is_zero x then 0
  else
    match x.limbs with
    | [| l |] -> if x.sign >= 0 then l else -l
    | _ ->
        let acc = x.limbs.(0) in
        if x.sign >= 0 then acc else -acc

let cmp_mag a b =
  let la = Array.length a in
  let lb = Array.length b in
  if la <> lb then compare la lb
  else
    let rec loop i =
      if i < 0 then 0
      else
        let ca = a.(i) and cb = b.(i) in
        if ca <> cb then if ca > cb then 1 else -1 else loop (i - 1)
    in
    loop (la - 1)

let compare a b =
  if a.sign = b.sign then
    match a.sign with
    | 0 -> 0
    | 1 -> cmp_mag a.limbs b.limbs
    | -1 -> -cmp_mag a.limbs b.limbs
    | _ -> assert false
  else compare a.sign b.sign

let equal a b = compare a b = 0
let gt a b = compare a b > 0
let lt a b = compare a b < 0
let ge a b = compare a b >= 0
let le a b = compare a b <= 0
let neg x = if x.sign = 0 then x else { x with sign = -x.sign }
let abs x = if x.sign >= 0 then x else neg x
let make_from_sign_and_limbs s limbs = normalize { sign = s; limbs }

let add_mag a b =
  let la = Array.length a and lb = Array.length b in
  let lr = if la >= lb then la else lb in
  let r = make (lr + 1) in
  let carry = ref 0 in
  let i = ref 0 in
  while !i < lr do
    let va = if !i < la then a.(!i) else 0 in
    let vb = if !i < lb then b.(!i) else 0 in
    let s = va + vb + !carry in
    r.(!i) <- s land base_mask;
    carry := s lsr limb_bits;
    incr i
  done;
  if !carry <> 0 then r.(lr) <- !carry;
  r

let sub_mag a b =
  let la = Array.length a and lb = Array.length b in
  let r = make la in
  let borrow = ref 0 in
  let i = ref 0 in
  while !i < la do
    let va = a.(!i) in
    let vb = if !i < lb then b.(!i) else 0 in
    let tmp = va - vb - !borrow in
    if tmp < 0 then (
      r.(!i) <- (tmp + base) land base_mask;
      borrow := 1)
    else (
      r.(!i) <- tmp land base_mask;
      borrow := 0);
    incr i
  done;
  r

let add a b =
  match (a.sign, b.sign) with
  | 0, _ -> b
  | _, 0 -> a
  | sa, sb when sa = sb ->
      let limbs = add_mag a.limbs b.limbs in
      make_from_sign_and_limbs sa limbs
  | _, _ ->
      let c = cmp_mag a.limbs b.limbs in
      if c = 0 then zero
      else if c > 0 then
        let limbs = sub_mag a.limbs b.limbs in
        make_from_sign_and_limbs a.sign limbs
      else
        let limbs = sub_mag b.limbs a.limbs in
        make_from_sign_and_limbs b.sign limbs

let sub a b = add a (neg b)

let mul a b =
  if a.sign = 0 || b.sign = 0 then zero
  else
    let la = Array.length a.limbs and lb = Array.length b.limbs in
    if la = 1 && lb = 1 then
      let prod = a.limbs.(0) * b.limbs.(0) in
      let low = prod land base_mask in
      let high = prod lsr limb_bits in
      if high = 0 then make_from_sign_and_limbs (a.sign * b.sign) [| low |]
      else make_from_sign_and_limbs (a.sign * b.sign) [| low; high |]
    else if la = 1 || lb = 1 then (
      let small, big, sgn =
        if la = 1 then (a.limbs.(0), b.limbs, a.sign * b.sign)
        else (b.limbs.(0), a.limbs, a.sign * b.sign)
      in
      let n = Array.length big in
      let r = make (n + 1) in
      let carry = ref 0 in
      for i = 0 to n - 1 do
        let prod = (big.(i) * small) + !carry in
        r.(i) <- prod land base_mask;
        carry := prod lsr limb_bits
      done;
      if !carry <> 0 then r.(n) <- !carry;
      make_from_sign_and_limbs sgn r)
    else
      let r = make (la + lb) in
      for i = 0 to la - 1 do
        let carry = ref 0 in
        let ai = a.limbs.(i) in
        for j = 0 to lb - 1 do
          let idx = i + j in
          let prod = r.(idx) + (ai * b.limbs.(j)) + !carry in
          r.(idx) <- prod land base_mask;
          carry := prod lsr limb_bits
        done;
        if !carry <> 0 then r.(i + lb) <- (!carry + r.(i + lb)) land base_mask
      done;
      make_from_sign_and_limbs (a.sign * b.sign) r

let divrem_small a (d : int) =
  if d <= 0 then invalid_arg "divrem_small: d must be > 0";
  if a.sign = 0 then (zero, 0)
  else
    let n = Array.length a.limbs in
    let q = make n in
    let rem = ref 0 in
    for i = n - 1 downto 0 do
      let cur = (!rem lsl limb_bits) + a.limbs.(i) in
      let qi = cur / d in
      q.(i) <- qi;
      rem := cur - (qi * d)
    done;
    (make_from_sign_and_limbs (if a.sign >= 0 then 1 else -1) q, !rem)

let div_rem a b =
  if b.sign = 0 then invalid_arg "div_rem: division by zero";
  if a.sign = 0 then (zero, zero)
  else
    let cmp = cmp_mag a.limbs b.limbs in
    if cmp < 0 then (zero, a)
    else if Array.length b.limbs = 1 then
      let q_small, r = divrem_small a b.limbs.(0) in
      let q = if a.sign * b.sign >= 0 then q_small else neg q_small in
      let r_t = of_int r in
      let r_t = if a.sign < 0 then neg r_t else r_t in
      if r = 0 then (q, zero) else (q, r_t)
    else
      let n = Array.length b.limbs in
      let m = Array.length a.limbs - n in
      let u = make (Array.length a.limbs + 1) in
      Array.blit a.limbs 0 u 0 (Array.length a.limbs);
      u.(Array.length a.limbs) <- 0;
      let v = Array.copy b.limbs in

      let top = v.(n - 1) in
      let rec bits_needed x k =
        if x >= 1 lsl (limb_bits - 1) then k else bits_needed (x lsl 1) (k + 1)
      in
      let s = bits_needed top 0 in

      let shift_left_bits arr s =
        if s = 0 then Array.copy arr
        else
          let len = Array.length arr in
          let res = make (len + 1) in
          let carry = ref 0 in
          for i = 0 to len - 1 do
            let cur = (arr.(i) lsl s) + !carry in
            res.(i) <- cur land base_mask;
            carry := cur lsr limb_bits
          done;
          res.(len) <- !carry;
          res
      in
      let shift_right_bits arr s =
        if s = 0 then Array.copy arr
        else
          let len = Array.length arr in
          let res = make len in
          let carry = ref 0 in
          for i = len - 1 downto 0 do
            let cur = (arr.(i) lsr s) lor (!carry lsl (limb_bits - s)) in
            res.(i) <- cur land base_mask;
            carry := arr.(i) land ((1 lsl s) - 1)
          done;
          res
      in

      let u' = shift_left_bits u s in
      let v' = shift_left_bits v s in

      let q = make (m + 1) in

      let v_n1 = v'.(n - 1) in
      let v_n2 = if n - 2 >= 0 then v'.(n - 2) else 0 in

      for j = m downto 0 do
        let uj_n = u'.(j + n) in
        let uj_n1 = u'.(j + n - 1) in
        let uj_n2 = if j + n - 2 >= 0 then u'.(j + n - 2) else 0 in

        let numerator = (uj_n lsl limb_bits) + uj_n1 in
        let qhat =
          let qh = numerator / v_n1 in
          if qh >= base then base - 1 else qh
        in

        let rec adjust qh =
          if qh = 0 then 0
          else
            let prod1 = qh * v_n2 in
            let left = prod1 in
            let r_temp = numerator - (qh * v_n1) in
            let r_shifted = r_temp lsl limb_bits in
            let rhs = r_shifted + uj_n2 in
            if left > rhs then adjust (qh - 1) else qh
        in

        let qhat = adjust qhat in

        let borrow = ref 0 in
        for i = 0 to n - 1 do
          let p = (qhat * v'.(i)) + !borrow in
          let idx = j + i in
          let diff = u'.(idx) - (p land base_mask) in
          let carry_p = p lsr limb_bits in
          let sub = diff in
          if sub < 0 then (
            u'.(idx) <- (sub + base) land base_mask;
            borrow := carry_p + 1)
          else (
            u'.(idx) <- sub land base_mask;
            borrow := carry_p)
        done;

        let diff2 = u'.(j + n) - !borrow in
        u'.(j + n) <- diff2 land base_mask;
        if diff2 < 0 then (
          q.(j) <- qhat - 1;
          let carry2 = ref 0 in
          for i = 0 to n - 1 do
            let idx = j + i in
            let sum = u'.(idx) + v'.(i) + !carry2 in
            u'.(idx) <- sum land base_mask;
            carry2 := sum lsr limb_bits
          done;
          u'.(j + n) <- (u'.(j + n) + !carry2) land base_mask)
        else q.(j) <- qhat
      done;

      let r_unshifted = shift_right_bits u' s in
      let rem_len = n in
      let rem_arr = make rem_len in
      Array.blit r_unshifted 0 rem_arr 0 rem_len;

      let q_t =
        normalize
          (make_from_sign_and_limbs (if a.sign * b.sign >= 0 then 1 else -1) q)
      in
      let r_t = normalize (make_from_sign_and_limbs a.sign rem_arr) in
      (q_t, r_t)

let div a b = fst (div_rem a b)

let rem a b =
  let _, r = div_rem a b in
  r

let pow a n =
  if n < 0 then invalid_arg "pow: negative exponent"
  else if n = 0 then one
  else if n = 1 then a
  else
    let rec aux acc base e =
      if e = 0 then acc
      else if e land 1 = 1 then aux (mul acc base) (mul base base) (e lsr 1)
      else aux acc (mul base base) (e lsr 1)
    in
    aux one a n

let to_twos_comp x k =
  let res = make k in
  if x.sign >= 0 then (
    Array.blit x.limbs 0 res 0 (min (Array.length x.limbs) k);
    res)
  else
    let mag = make k in
    Array.blit x.limbs 0 mag 0 (min (Array.length x.limbs) k);
    for i = 0 to k - 1 do
      mag.(i) <- base_mask lxor mag.(i)
    done;
    let carry = ref 1 in
    for i = 0 to k - 1 do
      let sum = mag.(i) + !carry in
      mag.(i) <- sum land base_mask;
      carry := sum lsr limb_bits
    done;
    mag

let of_twos_comp arr =
  let k = Array.length arr in
  if k = 0 then zero
  else
    let highest = arr.(k - 1) in
    let negative = highest lsr (limb_bits - 1) = 1 in
    if not negative then make_from_sign_and_limbs 1 (Array.copy arr)
    else
      let mag = Array.copy arr in
      for i = 0 to k - 1 do
        mag.(i) <- base_mask lxor mag.(i)
      done;
      let carry = ref 1 in
      for i = 0 to k - 1 do
        let sum = mag.(i) + !carry in
        mag.(i) <- sum land base_mask;
        carry := sum lsr limb_bits
      done;
      make_from_sign_and_limbs (-1) mag

let bitwise_op a b f =
  let la = Array.length a.limbs and lb = Array.length b.limbs in
  let k = max la lb + 1 in
  let ta = to_twos_comp a k in
  let tb = to_twos_comp b k in
  let res = make k in
  for i = 0 to k - 1 do
    res.(i) <- f ta.(i) tb.(i)
  done;
  of_twos_comp res

let logand a b = bitwise_op a b (fun x y -> x land y)
let logor a b = bitwise_op a b (fun x y -> x lor y)
let logxor a b = bitwise_op a b (fun x y -> x lxor y)

let lognot a =
  let la = Array.length a.limbs in
  let k = la + 1 in
  let ta = to_twos_comp a k in
  let res = make k in
  for i = 0 to k - 1 do
    res.(i) <- base_mask lxor ta.(i)
  done;
  of_twos_comp res

let shift_left a s =
  if is_zero a then a
  else if s = 0 then a
  else
    let limb_shift = s / limb_bits in
    let bit_shift = s mod limb_bits in
    let la = Array.length a.limbs in
    let res = make (la + limb_shift + 1) in
    let carry = ref 0 in
    for i = 0 to la - 1 do
      let cur = (a.limbs.(i) lsl bit_shift) + !carry in
      res.(i + limb_shift) <- cur land base_mask;
      carry := cur lsr limb_bits
    done;
    if !carry <> 0 then res.(la + limb_shift) <- !carry;
    make_from_sign_and_limbs a.sign res

let shift_right a s =
  if is_zero a then a
  else if s = 0 then a
  else
    let limb_shift = s / limb_bits in
    let bit_shift = s mod limb_bits in
    let la = Array.length a.limbs in
    if limb_shift >= la then if a.sign >= 0 then zero else minus_one
    else
      let new_len = la - limb_shift in
      let res = make new_len in
      let carry = ref 0 in
      for i = la - 1 downto limb_shift do
        let v = a.limbs.(i) in
        let shifted =
          (v lsr bit_shift) lor (!carry lsl (limb_bits - bit_shift))
        in
        res.(i - limb_shift) <- shifted land base_mask;
        carry := v land ((1 lsl bit_shift) - 1)
      done;
      if a.sign < 0 then (
        let ta = to_twos_comp a la in
        let ta2 = make la in
        for i = la - 1 downto 0 do
          let src_idx = i + limb_shift in
          if src_idx < la then
            let low = ta.(src_idx) lsr bit_shift in
            let high =
              if src_idx + 1 < la then
                (ta.(src_idx + 1) land ((1 lsl bit_shift) - 1))
                lsl (limb_bits - bit_shift)
              else 0
            in
            ta2.(i) <- low lor high land base_mask
          else ta2.(i) <- base_mask
        done;
        of_twos_comp ta2)
      else make_from_sign_and_limbs a.sign res

let of_string s =
  let len = String.length s in
  if len = 0 then invalid_arg "of_string: empty";
  let pos = ref 0 in
  let sign = ref 1 in
  if s.[0] = '-' then (
    sign := -1;
    pos := 1)
  else if s.[0] = '+' then pos := 1;
  let res = ref zero in
  while !pos < len do
    let rem = len - !pos in
    let take = if rem mod 9 = 0 then 9 else rem mod 9 in
    let chunk = int_of_string (String.sub s !pos take) in
    let mult = pow (of_int 10) take in
    res := add (mul !res mult) (of_int chunk);
    pos := !pos + take
  done;
  if !sign >= 0 then !res else neg !res

let to_string x =
  if is_zero x then "0"
  else
    let sign_pref = if x.sign < 0 then "-" else "" in
    let base10_chunk = 1_000_000_000 in
    let rec loop n acc =
      if is_zero n then acc
      else
        let q, r_small = divrem_small n base10_chunk in
        loop q (Int.to_string r_small :: acc)
    in
    let chunks = loop (abs x) [] in
    match chunks with
    | [] -> "0"
    | h :: t ->
        let formatted =
          h
          ^ String.concat ""
              (List.map (fun s -> String.make (9 - String.length s) '0' ^ s) t)
        in
        sign_pref ^ formatted

let to_float x =
  if is_zero x then 0.0
  else
    let res = ref 0.0 in
    for i = Array.length x.limbs - 1 downto 0 do
      res := (!res *. float_of_int base) +. float_of_int x.limbs.(i)
    done;
    if x.sign < 0 then -. !res else !res

let of_float f =
  if f = 0.0 then zero
  else
    let s = if f < 0.0 then -1 else 1 in
    let v = abs_float f in
    let rec build v acc =
      if v < 1.0 then acc
      else
        let limb = int_of_float (mod_float v (float_of_int base)) in
        build (floor (v /. float_of_int base)) (limb :: acc)
    in
    let limbs_list = build v [] in
    let limbs = Array.of_list (List.rev limbs_list) in
    normalize { sign = s; limbs }

let to_int64 x =
  if is_zero x then 0L
  else
    match x.limbs with
    | [| l |] ->
        if x.sign >= 0 then Int64.of_int l else Int64.neg (Int64.of_int l)
    | [| lo; hi |] ->
        let v =
          Int64.add
            (Int64.shift_left (Int64.of_int hi) limb_bits)
            (Int64.of_int lo)
        in
        if x.sign >= 0 then v else Int64.neg v
    | _ -> failwith "to_int64: overflow"

let pp fmt b = Format.fprintf fmt "%s" (to_string b)
