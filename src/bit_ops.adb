package body Bit_Ops is
   function Bitwise_Or (X, Y : Byte) return Byte is
      X_Bits : Pixel with Address => X'Address;
      Y_Bits : Pixel with Address => Y'Address;
      Bits : aliased Pixel;
      Bits_Byte : Byte with Address => Bits'Address;
   begin
      Bits := X_Bits or Y_Bits;
      return Bits_Byte;
   end Bitwise_Or;

   function Bitwise_And (X, Y : Byte) return Byte is
      X_Bits : Pixel with Address => X'Address;
      Y_Bits : Pixel with Address => Y'Address;
      Bits : aliased Pixel;
      Bits_Byte : Byte with Address => Bits'Address;
   begin
      Bits := X_Bits and Y_Bits;
      return Bits_Byte;
   end Bitwise_And;

   function Bitwise_Xor (X, Y : Byte) return Byte is
      X_Bits : Pixel with Address => X'Address;
      Y_Bits : Pixel with Address => Y'Address;
      Bits : aliased Pixel;
      Bits_Byte : Byte with Address => Bits'Address;
   begin
      Bits := X_Bits xor Y_Bits;
      return Bits_Byte;
   end Bitwise_Xor;
end Bit_Ops;
