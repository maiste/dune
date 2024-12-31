open Import

(** [Lazy.force bin] returns the path or triggers an error if the
    curl executable can't be found. *)
val bin : Path.t Lazy.t

(** [run ~url ~temp_dir ~output] will download the file from [url]
    to [output] using [temp_dir] to keep log files. *)
val run
  :  url:string
  -> temp_dir:Path.t
  -> output:Path.t
  -> (unit, User_message.t) result Fiber.t
