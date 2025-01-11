with ISA;

package Bit_Ops is
   use ISA;

   function Bitwise_Or  (X, Y : Byte) return Byte;
   function Bitwise_And (X, Y : Byte) return Byte;
   function Bitwise_Xor (X, Y : Byte) return Byte;
end Bit_Ops;
