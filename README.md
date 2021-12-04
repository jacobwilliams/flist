Modern Fortran Linked List

### Description

Just some experiments with modern Fortran linked lists and unlimited polymorphic derived types.

### Compiling

A [Fortran Package Manager](https://github.com/fortran-lang/fpm) manifest file is included, so that the library and tests cases can be compiled with FPM. For example:

```
fpm build --profile release
fpm test --profile release
```

To use `flist` within your fpm project, add the following to your `fpm.toml` file:
```toml
[dependencies]
flist = { git="https://github.com/jacobwilliams/flist.git" }
```

### License

The `flist` source code and related files and documentation are distributed under a permissive free software [license](https://github.com/jacobwilliams/flist/blob/master/LICENSE) (BSD-3).

