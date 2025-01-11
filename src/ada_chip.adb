with Ada.Command_Line;
with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;
with Sf;

with ISA; use ISA;
with CPU;
with Video;

procedure Ada_Chip is
   package Random_Byte is new Ada.Numerics.Discrete_Random (Byte);

   Steps_Per_Frame : constant := 8;

   State            : CPU.Instance;
   Random_Generator : Random_Byte.Generator;

   procedure Draw_Sprite (VX, VY : Register_Index; N : Byte) is
      use Sf;

      X, Y       : sfUint32;
      Row        : aliased Byte;
      Row_Pixels : Pixel with Address => Row'Address;
      VF         : Boolean := False;
   begin
      X := sfUint32 (State.Registers (VX));
      Y := sfUint32 (State.Registers (VY));

      for I in 0 .. N - 1 loop
         Row := State.Memory (State.Address_Register + Address (I));

         for J in 0 .. 7 loop
            if Row_Pixels (7 - J) then
               if Video.Toggle_Pixel
                  ((X + sfUint32 (J)) mod Video.Width,
                     (Y + sfUint32 (I)) mod Video.Height)
               then
                  VF := True;
               end if;
            end if;
         end loop;
      end loop;

      State.Registers (15) := (if VF then 1 else 0);
   end Draw_Sprite;

   procedure Run_Step is
      ins : Opcode;
   begin
      ins := CPU.Get_Opcode (State);
      case ins.Class is
         when Flow => case ins.Value is
            when 16#E0# => Video.Clear_Screen;
            when 16#EE# => CPU.Ret (State);
            when others => begin
               Ada.Text_IO.Put_Line ("Unknown flow instruction!");
               Ada.Text_IO.Put_Line (Opcode_Value'Image (ins.Value));
               delay 1.0;
               Video.Finish;
            end;
         end case;
         when Jump => CPU.Jump (State, Address (ins.Value));
         when Call => CPU.Call (State, Address (ins.Value));
         when Equal =>
            if State.Registers (X_Register (ins)) = To_Byte (ins) then
               CPU.Skip (State);
            end if;
         when Not_Equal =>
            if State.Registers (X_Register (ins)) /= To_Byte (ins) then
               CPU.Skip (State);
            end if;
         when Compare =>
            if State.Registers (X_Register (ins)) =
               State.Registers (Y_Register (ins))
            then
               CPU.Skip (State);
            end if;
         when Set_Register =>
            State.Registers (X_Register (ins)) := To_Byte (ins);
         when Add =>
            State.Registers (X_Register (ins)) :=
               State.Registers (X_Register (ins)) + To_Byte (ins);
         when Math =>
            CPU.Math (State, X_Register (ins), Y_Register (ins),
               To_Byte (ins) mod 16);
         when Not_Compare =>
            if State.Registers (X_Register (ins)) /=
               State.Registers (Y_Register (ins))
            then
               CPU.Skip (State);
            end if;
         when Set_Address =>
            State.Address_Register := Address (ins.Value);
         when Random =>
            State.Registers (X_Register (ins)) :=
               Random_Byte.Random (Random_Generator) mod To_Byte (ins);
         when Draw_Sprite =>
            Draw_Sprite (X_Register (ins), Y_Register (ins),
               To_Byte (ins) mod 16);
         when Input => case To_Byte (ins) is
            when 16#9E# =>
               if Video.Key_Down
                  (Video.Key (State.Registers (X_Register (ins)) mod 16))
               then
                  CPU.Skip (State);
               end if;
            when 16#A1# =>
               if Video.Key_Up
                  (Video.Key (State.Registers (X_Register (ins)) mod 16))
               then
                  CPU.Skip (State);
               end if;
            when others => null;
         end case;
         when Misc => case To_Byte (ins) is
            when 16#07# =>
               State.Registers (X_Register (ins)) := State.Delay_Timer;
            when 16#0A# =>
               State.Registers (X_Register (ins)) := Byte (Video.Next_Key);
            when 16#15# =>
               State.Delay_Timer := State.Registers (X_Register (ins));
            when 16#18# => null; --  TODO: sound
            when 16#1E# =>
               State.Address_Register := State.Address_Register +
                  Address (State.Registers (X_Register (ins)));
            when 16#29# =>
               State.Address_Register :=
                  Address (State.Registers (X_Register (ins)) mod 16) * 5;
            when 16#33# => begin
               State.Memory (State.Address_Register) :=
                  State.Registers (X_Register (ins)) / 100;
               State.Memory (State.Address_Register + 1) :=
                  State.Registers (X_Register (ins)) / 10 mod 10;
               State.Memory (State.Address_Register + 2) :=
                  State.Registers (X_Register (ins)) mod 10;
            end;
            when 16#55# =>
               CPU.Reg_Store (State, X_Register (ins));
            when 16#65# =>
               CPU.Reg_Load (State, X_Register (ins));
            when others => begin
               Ada.Text_IO.Put_Line ("Unknown misc instruction!");
               Ada.Text_IO.Put_Line (Opcode_Value'Image (ins.Value));
               delay 1.0;
               Video.Finish;
            end;
         end case;
         when others => begin
            Ada.Text_IO.Put_Line ("Unknown instruction class!");
            Ada.Text_IO.Put_Line (Opcode_Class'Image (ins.Class));
            delay 1.0;
            Video.Finish;
         end;
      end case;
   end Run_Step;
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put_Line ("usage: adachip <.c8 file>");
   else
      Video.Initialize;
      Random_Byte.Reset (Random_Generator);
      CPU.Load_File (State, Ada.Command_Line.Argument (1));

      while Video.Is_Running loop
         Video.Display;
         Video.Poll_Events;

         if State.Delay_Timer > 0 then
            State.Delay_Timer := State.Delay_Timer - 1;
         end if;

         for I in 0 .. Steps_Per_Frame loop
            Run_Step;
         end loop;
      end loop;
   end if;
end Ada_Chip;
