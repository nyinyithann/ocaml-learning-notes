# Random Notes
### local opam switch
- `Opam switch ./ ocaml-base-compiler.4.14.0`
- `opam exec -- dune exec ./bin/main.exe` or `dune exec -- helloworld`
<br/><br/>
### Janestreet Core
#### Core old names vs Standalone new names

| Old name      | New name      |
| ------------- | ------------- |
| Core.Command  | Command_unix  |
| Core.Date     | Date_unix     |
| Core.Filename | Filename_unix |
| Core.Signal   | Signal_unix   |
| Core.Sys      | Sys_unix      |
| Core.Thread   | Core_thread   |
| Core.Time     | Time_unix     |
| Core.Time_ns  | Time_ns_unix  |
| Core.Unix     | Core_unix     |

Ref: https://ocaml.janestreet.com/ocaml-core/v0.12/doc/index.html