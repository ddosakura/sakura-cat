source_filename = "test1"

@.str-0 = global [5 x i8] c"Hello"
@.str-1 = global [6 x i8] c"World!"
@.str-2 = global [1 x i8] c" "
@.str-3 = global [1 x i8] c"\0A"

define void @foo() {
; <label>:0
	call void @print(i8* getelementptr ([5 x i8], [5 x i8]* @.str-0, i32 0, i32 0), i32 5)
	call void @print(i8* getelementptr ([1 x i8], [1 x i8]* @.str-2, i32 0, i32 0), i32 1)
	call void @print(i8* getelementptr ([6 x i8], [6 x i8]* @.str-1, i32 0, i32 0), i32 6)
	call void @print(i8* getelementptr ([1 x i8], [1 x i8]* @.str-3, i32 0, i32 0), i32 1)
	ret void
}

declare void @print(i8*, i32)

define void @main() {
; <label>:0
	call void @foo()
	%1 = alloca i32
	store i32 1, i32* %1
	%2 = alloca i32
	store i32 0, i32* %2
	ret void
}
