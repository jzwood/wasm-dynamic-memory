(module
  (import "console" "log" (func $log (param i32)))
  (memory (export "memory") 1)
  (global $bytes_per_page (mut i32) (i32.const 65536))
  (global $span (mut i32) (i32.const 0))

  ;;   >----------------->------------------v-------------------------------<
  ;;  next size (i31) | next free? (i1) | next data (i8 * n) | prev ptr (i31) | prev free? (i1) | next size (i31) | ...
  ;;   ^---------------------------------------------------<
  ;;




  ;; | free (1 bit) | size                       | data (size bytes) | prev free (1 bit) | offset (size + size byte length) |
  ;; |--------------+----------------------------+-------------------+-------------------+----------------------------------|
  ;; | x            | 0xxxxxx (6 bits)           |                   | x                 | 0xxxxxx (6 bits)                 |
  ;; | x            | 10xxxxx (5 bits + 1 byte)  |                   | x                 | 10xxxxx (5 bits + 1 byte)        |
  ;; | x            | 110xxxx (4 bits + 2 bytes) |                   | x                 | 110xxxx (4 bits + 2 bytes)       |
  ;; | x            | 1110xxx (3 bits + 3 bytes) |                   | x                 | 1110xxx (3 bits + 3 bytes)       |
  ;; | x            | 11110xx (2 bits + 4 bytes) |                   | x                 | 11110xx (2 bits + 4 bytes)       |
  ;;
  ;; malloc:
  ;; use next_ptr to jump blocks until you reach an free block
  ;; if node is smaller than requested block:
  ;;    update next size
  ;;    set next free = 1
  ;;
  ;;    compare pointer current pointer with next_ptr to see if empty block is big enough
  ;; else jump to next node and repeat from top
  ;;

  (func $inspect (param $x i32) (result i32)
    local.get $x
    call $log
    local.get $x
  )

  (func (export "init")
    (local $ptr i32)
    i32.const 0 ;; pointer 0

    global.get $bytes_per_page
    i32.const 1
    i32.sub
    local.tee $ptr
    i32.const 0
    call $encode
  )

  (func $encode (param $ptr i32) (param $free? i32) (result i32)
    local.get $ptr
    i32.const 1
    i32.shl
    local.get $free?
    i32.or
  )

  (func $decode (param $node i32) (result i32 i32)
    local.get $node
    i32.const 1
    i32.shr_u

    local.get $node
    i32.const 1
    i32.and
  )

  (func $malloc (export "malloc") (param $size i32) (result i32)
    i32.const 0
  )

  (func $free (export "free") (param $ptr i32))

)
