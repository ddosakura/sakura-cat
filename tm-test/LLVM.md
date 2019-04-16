docs: http://llvm.org/docs/index.html

+ clang: C语言编译器，类似于gcc 
+ clang++: C++编译器，类似于g++。clang++只是clang的一个别名。 
+ clang-format：按照固定的规范格式化C/C++代码，非常智能。文档请见：http://clang.llvm.org/docs/ClangFormat.html 
+ clang-modernize：把按照C++98标准写的代码，转成C++11标准的。文档请见：http://clang.llvm.org/extra/ModernizerUsage.html 
+ llvm-as：LLVM 汇编器 
+ llvm-dis： LLVM 反汇编器 
+ opt：LLVM 优化器 
+ llc：LLVM 静态编译器 
+ lli：LLVM的字节码执行器（某些平台下支持JIT） 
+ llvm-link：LLVM的字节码链接器 
+ llvm-ar：LLVM的静态库打包器，类似unix的ar。 
+ llvm-nm：类似于unix的nm 
+ llvm-ranlib:为 llvm-ar 打包的文件创建索引 
+ llvm-prof：将 ‘llvmprof.out’ raw 数据格式化成人类可读的报告 
+ llvm-ld ：带有可装载的运行时优化支持的通用目标连接器 
+ llvm-config：打印出配置时 LLVM 编译选项、库、等等 
+ llvmc：一个通用的可定制的编译器驱动 
+ llvm-diff：比较两个模块的结构 
+ bugpoint：自动案例测试减速器 
+ llvm-extract：从 LLVM 字节代码文件中解压出一个函数 
+ llvm-bcanalyzer：字节代码分析器 （分析二进制编码本身，而不是它代表的程序） 
+ FileCheck：灵活的文件验证器，广泛的被测试工具利用 
+ tblgen：目标描述阅读器和生成器 
+ lit：LLVM 集成测试器，用于运行测试

$ llvm-
llvm-ar
llvm-cov
llvm-diff
llvm-exegesis
llvm-mc
llvm-objdump
llvm-rc
llvm-stress
llvm-xray
llvm-as
llvm-c-test
llvm-dis
llvm-extract
llvm-mca
llvm-opt-report
llvm-readelf
llvm-strings
llvm-bcanalyzer
llvm-cvtres
llvm-dlltool
llvm-lib
llvm-modextract
llvm-pdbutil
llvm-readobj
llvm-strip
llvm-cat
llvm-cxxdump
llvm-dwarfdump
llvm-link
llvm-mt
llvm-PerfectShuffle
llvm-rtdyld
llvm-symbolizer
llvm-cfi-verify
llvm-cxxfilt
llvm-dwp
llvm-lto
llvm-nm
llvm-profdata
llvm-size
llvm-tblgen
llvm-config
llvm-cxxmap
llvm-elfabi
llvm-lto2
llvm-objcopy
llvm-ranlib
llvm-split
llvm-undname