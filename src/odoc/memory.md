# Memory usage notes

These are some notes on the memory usage of odoc. I'm particularly focusing on `core_kernel` here, and furthermore on
the `link` phase, which has been noted as taking a large amount of memory.

## Lower bound

First let's get some bounds on how much memory usage is inevitable. The output from the link stage is currently written
to disk as an `.odocl` file. For core_kernel this is a 70MiB file:

```
# ls -l ~/.opam/4.09.0/var/cache/odig/odoc/core_kernel/core_kernel.odocl
-rw-r--r--  1 jon  staff  72930088 16 Jun 13:10 /Users/jon/.opam/4.09.0/var/cache/odig/odoc/core_kernel/core_kernel.odocl
```

When this is loaded into ocaml it seems to take up quite a lot more memory:

```
utop # Gc.((stat ()).live_words);;
- : int = 1534215
utop # let f = Odoc_odoc.Fs.File.of_string "/Users/jon/.opam/4.09.0/var/cache/odig/odoc/core_kernel/core_kernel.odocl";;
val f : Odoc_odoc.Fs.file = <abstr>
utop # let x = Odoc_odoc.Compilation_unit.load f;;
val x : (Odoc_odoc.Compilation_unit.t, [> Odoc_odoc.Or_error.msg ]) result =
  Odoc_odoc.Or_error.Ok
   {Odoc_model.Lang.Compilation_unit.id =
...
utop # Gc.stat ();;
- : Gc.stat =
{Gc.minor_words = 19253064.; promoted_words = 1801524.;
 major_words = 53715780.; minor_collections = 93; major_collections = 10;
 heap_words = 85848576; heap_chunks = 15; live_words = 47027933;
 live_blocks = 15595262; free_words = 38820478; free_blocks = 8756;
 largest_free = 35046767; fragments = 165; compactions = 0;
 top_heap_words = 85848576; stack_size = 95}
 ```

 so we can see here live_words has increased from 1534215 to 47027933, an increase of 45493718 words,
 or 350MiB. `heap_words` has also hit 85848576 words, or 654MiB, of which 603M is resident. So the
 minimum possible memory usage of linking core_kernel is going to be of this order of magnitude.

 ## Linking dependencies

 Next let's see how much space is used in loading the individual `.odoc` files used in the generation
 of `core_kernel.odocl`. This can be checked by running the `link` phase until the initial environment
 has been created, which by side effect loads in all of the dependent odoc files. The `live_words` at
 this point is 38654582, or 300MiB. If we keep all of these in memory as we run, when we finally
 create the linked data structure we'll have the sum of this value and the size of the final data
 structure, giving us approximage 700MiB.

 The structure of `Core_kernel` the module is roughly that it is a long list of module aliases. We
 can modify the link phase such that after calculating the expansion of each module we can check
 the `live_words` value. 


