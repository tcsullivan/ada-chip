with Ada.Containers.Vectors;
with ISA;

package CPU is
   use ISA;

   package Address_Stack is new Ada.Containers.Vectors
      (Index_Type => Natural, Element_Type => Address);

   type Instance is record
      Memory           : Bank := (
         16#F0#, 16#90#, 16#90#, 16#90#, 16#F0#, -- 0
         16#20#, 16#60#, 16#20#, 16#20#, 16#70#, -- 1
         16#F0#, 16#10#, 16#F0#, 16#80#, 16#F0#, -- 2
         16#F0#, 16#10#, 16#F0#, 16#10#, 16#F0#, -- 3
         16#90#, 16#90#, 16#F0#, 16#10#, 16#10#, -- 4
         16#F0#, 16#80#, 16#F0#, 16#10#, 16#F0#, -- 5
         16#F0#, 16#80#, 16#F0#, 16#90#, 16#F0#, -- 6
         16#F0#, 16#10#, 16#20#, 16#40#, 16#40#, -- 7
         16#F0#, 16#90#, 16#F0#, 16#90#, 16#F0#, -- 8
         16#F0#, 16#90#, 16#F0#, 16#10#, 16#F0#, -- 9
         16#F0#, 16#90#, 16#F0#, 16#90#, 16#90#, -- A
         16#E0#, 16#90#, 16#E0#, 16#90#, 16#E0#, -- B
         16#F0#, 16#80#, 16#80#, 16#80#, 16#F0#, -- C
         16#E0#, 16#90#, 16#90#, 16#90#, 16#E0#, -- D
         16#F0#, 16#80#, 16#F0#, 16#80#, 16#F0#, -- E
         16#F0#, 16#80#, 16#F0#, 16#80#, 16#80#, -- F
         others => 0
      );
      Registers        : Register_Bank;
      Program_Counter  : Address := Start_Address;
      Address_Register : Address := 0;
      Stack            : Address_Stack.Vector;
      Delay_Timer      : Byte := 0;
   end record;

   procedure Load_File (Inst : in out Instance; File_Name : String);

   function Get_Opcode (Inst : in out Instance) return Opcode;

   procedure Reg_Store (Inst : in out Instance; VX : Register_Index);
   procedure Reg_Load (Inst : in out Instance; VX : Register_Index);
   procedure Ret (Inst : in out Instance);
   procedure Call (Inst : in out Instance; A : Address);
   procedure Jump (Inst : in out Instance; A : Address);
   procedure Skip (Inst : in out Instance);
   procedure Math (Inst : in out Instance; VX, VY : Register_Index; N : Byte);
end CPU;
