(module
  (import "console" "log" (func $log (param i32)))
  (memory (export "memory") 1)
  (global $width (mut i32) (i32.const 0))

  (func $inspect (param $x i32) (result i32)
    local.get $x
    call $log
    local.get $x
  )

  (func $malloc (export "malloc") (param $size i32) (result $ptr i32))

  (func $free (export "free") (param $ptr))

)
