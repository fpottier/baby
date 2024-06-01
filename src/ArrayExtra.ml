open Array

let compress equal a =
  let n = length a in
  if n <= 1 then
    (* If the length of the array is 0 or 1, there is nothing to do. *)
    n
  else
    (* Now, prepare to copy the elements of the array downwards, in place,
       so as to eliminate duplicate elements. [last] is the last element
       that was read and kept. [!dst] is the number of elements that have
       been kept so far, so it is also the index of the slot that will be
       written next. *)
    let last = ref a.(0) in
    let dst = ref 1 in
    (* Let [src] scan the array. *)
    for src = 1 to n - 1 do
      assert (1 <= !dst && !dst <= src && src < n);
      assert (equal !last a.(src-1));
      let current = a.(src) in
      if current = !last then
        (* Skip this element. *)
        ()
      else begin
        (* Keep this element, copying it down to index [!dst]. *)
        if !dst < src then a.(!dst) <- current;
        last := current;
        incr dst
      end;
      assert (equal !last current);
      assert (equal !last a.(src));
    done;
    !dst
