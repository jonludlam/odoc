open Odoc_utils

val build_hierarchies :
  warnings_options:Odoc_model.Error.warnings_options ->
  occurrences:Fs.file option ->
  roots:Fs.Directory.t list ->
  inputs_in_file:Fs.file list ->
  odocls:Fs.file list ->
  ( Odoc_index.Skeleton.t list * Odoc_occurrences.Table.t option,
    [> msg ] )
  result

val compile :
  output:Fs.file ->
  warnings_options:Odoc_model.Error.warnings_options ->
  occurrences:Fs.file option ->
  roots:Fs.Directory.t list ->
  inputs_in_file:Fs.file list ->
  odocls:Fs.file list ->
  (unit, [> msg ]) result
