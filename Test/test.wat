(module
  ;; import the browser console object, you'll need to pass this in from JavaScript
  (import "console" "log" (func $log (param i32)))

  ;; create a function that takes in a number as a param,
  ;; and logs that number if it's not equal to 100.
  (func (export "log_if_not_100") (param $num i32)
    i32.const 0
    i32.const 0
    i32.const 0
    block $1 (result i32)
      i32.const 100
      br $1
    end
    drop
    drop
    drop
    drop
  )
)

