package body ISA is
   function To_Byte (O : Opcode) return Byte
      is (Byte (O.Value mod 256));

   function X_Register (O : Opcode) return Register_Index
      is (Register_Index (O.Value / 256));

   function Y_Register (O : Opcode) return Register_Index
      is (Register_Index (O.Value / 16 mod 16));
end ISA;
