(module
  (import "console" "log" (func $log (param i32)))
  (memory (export "memory") 1)
  (global $bytes_per_page (mut i32) (i32.const 65536))
  (global $span (mut i32) (i32.const 0))


  (func $inspect (param $x i32) (result i32)
    local.get $x
    call $log
    local.get $x
  )

  (func (export "init")
    (local $end i32)
    global.get $bytes_per_page
    i32.const 1
    i32.sub
    local.tee $end
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

  ;;  next (i32) | data (?) | prev (i32) | next (i32) | data (?) | ...
  ;;  next/prev (i32) = ptr (i31) <> free (i1)
  ;;
  ;;
  ;;
  (func $malloc (export "malloc") (param $size i32) (result i32)
    i32.const 0
  )

  (func $free (export "free") (param $ptr i32))

)
