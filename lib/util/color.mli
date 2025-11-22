(** ANSI escape codes for text formatting and colors in the terminal. *)

val reset : string
(** Reset all styles and colors to default. *)

val bold : string
(** Bold text style. *)

val dim : string
(** Dim/faint text style. *)

val italic : string
(** Italic text style. *)

val underline : string
(** Underlined text style. *)

val blink : string
(** Blinking text style (may not be supported in all terminals). *)

val reverse : string
(** Swap foreground and background colors. *)

val black : string
val red : string
val green : string
val yellow : string
val blue : string
val magenta : string
val cyan : string
val white : string
val bright_red : string
val bright_green : string
val bright_yellow : string
val bright_blue : string
val bright_magenta : string
val bright_cyan : string
