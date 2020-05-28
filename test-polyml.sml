use "cephes-sig.sml";
use "cephes-polyml.sml";

val _ = print ((Real.toString Cephes.PI) ^ "\n");
val _ = print ((Real.toString (Cephes.ndtri 0.975)) ^ "\n");
