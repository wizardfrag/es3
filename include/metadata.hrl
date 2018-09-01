-record(metadata, {
  id,
  chunks,
  size
}).

-define(metadata_to_map(Metadata), #{
  id => Metadata#metadata.id,
  % chunks => Metadata#metadata.chunks, %% Since this map goes externally,
  size => Metadata#metadata.size        %% we probably shouldn't expose our chunk metadata
}).