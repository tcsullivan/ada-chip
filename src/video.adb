with Sf.Window;
with Sf.Window.Event;
with Sf.Window.Keyboard;
with Sf.Graphics.Color;
with Sf.Graphics.Image;
with Sf.Graphics.RenderWindow;
with Sf.Graphics.Texture;

package body Video is
   procedure Clear_Screen is
   begin
      for X in 0 .. Width - 1 loop
         for Y in 0 .. Height - 1 loop
            Image.setPixel (Pixels, X, Y, Color.sfBlack);
         end loop;
      end loop;
   end Clear_Screen;

   procedure Scroll_Right is
      use Sf.Graphics.Color;

      SS  : constant sfUint32 := 4;
      Tmp : array (0 .. SS - 1) of sfColor;
   begin
      for J in 0 .. Height - 1 loop
         for I in 0 .. SS - 1 loop
            Tmp (I) := Image.getPixel (Pixels, Width - SS + I, J);
         end loop;
         for I in 0 .. Width - 5 loop
            Image.setPixel (Pixels, I + SS, J, Image.getPixel (Pixels, I, J));
         end loop;
         for I in 0 .. SS - 1 loop
            Image.setPixel (Pixels, I, J, Tmp (I));
         end loop;
      end loop;
   end Scroll_Right;

   procedure Scroll_Left is
      use Sf.Graphics.Color;

      SS  : constant sfUint32 := 4;
      Tmp : array (0 .. SS - 1) of sfColor;
   begin
      for J in 0 .. Height - 1 loop
         for I in 0 .. SS - 1 loop
            Tmp (I) := Image.getPixel (Pixels, I, J);
         end loop;
         for I in SS .. Width - 1 loop
            Image.setPixel (Pixels, I - SS, J, Image.getPixel (Pixels, I, J));
         end loop;
         for I in 0 .. SS - 1 loop
            Image.setPixel (Pixels, Width - SS + I, J, Tmp (I));
         end loop;
      end loop;
   end Scroll_Left;

   procedure Low_Res is
   begin
      Video.Width := 64;
      Video.Height := 32;
      Video.Scale := 10;
      Sprite.setScale (Pixels_Sprite, (Float (Scale), Float (Scale)));
   end Low_Res;

   procedure High_Res is
   begin
      Video.Width := 128;
      Video.Height := 64;
      Video.Scale := 5;
      Sprite.setScale (Pixels_Sprite, (Float (Scale), Float (Scale)));
   end High_Res;

   procedure Initialize (M : Model) is
   begin
      case M is
         when Chip_8 =>
            Width  := 64;
            Height := 32;
            Scale  := 10;
         when Super_Chip_10 =>
            Width  := 128;
            Height := 64;
            Scale  := 5;
      end case;

      app := RenderWindow.create ((Width * Scale, Height * Scale, 32), Title);
      Pixels := Image.create (Width, Height);
      Pixels_Texture := Texture.createFromImage (Pixels);

      Sprite.setTexture (Pixels_Sprite, Pixels_Texture);
      Sprite.setPosition (Pixels_Sprite, (Float (0), Float (0)));
      RenderWindow.setFramerateLimit (app, 60);
   end Initialize;

   function Is_Running return Boolean is
   begin
      return RenderWindow.isOpen (app) = sfTrue;
   end Is_Running;

   function Next_Key return Key is
   begin
      while Is_Running loop
         Poll_Events;

         for I in Key'First .. Key'Last loop
            if Keys (I) then
               return I;
            end if;
         end loop;

         delay 0.016;
      end loop;

      return 0; --  Only get here on program exit
   end Next_Key;

   procedure Poll_Events is
      use Sf.Window.Event;
      use Sf.Window.Keyboard;

      e : sfEvent;
   begin
      while RenderWindow.pollEvent (app, event => e) = sfTrue loop
         case e.eventType is
            when sfEvtClosed =>
               RenderWindow.close (app);
            when sfEvtKeyPressed => case e.key.code is
               when sfKeyNum0 => Keys (0) := True;
               when sfKeyNum1 => Keys (1) := True;
               when sfKeyNum2 => Keys (2) := True;
               when sfKeyNum3 => Keys (3) := True;
               when sfKeyNum4 => Keys (4) := True;
               when sfKeyNum5 => Keys (5) := True;
               when sfKeyNum6 => Keys (6) := True;
               when sfKeyNum7 => Keys (7) := True;
               when sfKeyNum8 => Keys (8) := True;
               when sfKeyNum9 => Keys (9) := True;
               when sfKeyA => Keys (10) := True;
               when sfKeyB => Keys (11) := True;
               when sfKeyC => Keys (12) := True;
               when sfKeyD => Keys (13) := True;
               when sfKeyE => Keys (14) := True;
               when sfKeyF => Keys (15) := True;
               when others => null;
            end case;
            when sfEvtKeyReleased => case e.key.code is
               when sfKeyNum0 => Keys (0) := False;
               when sfKeyNum1 => Keys (1) := False;
               when sfKeyNum2 => Keys (2) := False;
               when sfKeyNum3 => Keys (3) := False;
               when sfKeyNum4 => Keys (4) := False;
               when sfKeyNum5 => Keys (5) := False;
               when sfKeyNum6 => Keys (6) := False;
               when sfKeyNum7 => Keys (7) := False;
               when sfKeyNum8 => Keys (8) := False;
               when sfKeyNum9 => Keys (9) := False;
               when sfKeyA => Keys (10) := False;
               when sfKeyB => Keys (11) := False;
               when sfKeyC => Keys (12) := False;
               when sfKeyD => Keys (13) := False;
               when sfKeyE => Keys (14) := False;
               when sfKeyF => Keys (15) := False;
               when others => null;
            end case;
            when others => null;
         end case;
      end loop;
   end Poll_Events;

   procedure Display is
   begin
      RenderWindow.clear (app, Color.sfWhite);
      Texture.updateFromImage (Pixels_Texture, Pixels, 0, 0);
      RenderWindow.drawSprite (app, Pixels_Sprite);
      RenderWindow.display (app);
   end Display;

   function Toggle_Pixel (X, Y : sfUint32) return Boolean is
      use Color;

      P : sfColor;
      R : Boolean;
   begin
      P := Image.getPixel (Pixels, X, Y);
      R := P = sfWhite;
      Image.setPixel (Pixels, X, Y, (if R then sfBlack else sfWhite));
      return R;
   end Toggle_Pixel;

   procedure Finish is
   begin
      RenderWindow.close (app);
   end Finish;

   function Key_Down (K : Key) return Boolean is
   begin
      return Keys (K);
   end Key_Down;

   function Key_Up (K : Key) return Boolean is
   begin
      return not Keys (K);
   end Key_Up;
end Video;
