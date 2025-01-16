with Sf.Window;
with Sf.Window.Event;
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
      Last_Key := Unknown;

      while Is_Running and then Last_Key = Unknown loop
         Poll_Events;
         delay 0.016;
      end loop;

      return Last_Key;
   end Next_Key;

   procedure Poll_Events is
      use Sf.Window.Event;

      e : sfEvent;
   begin
      while RenderWindow.pollEvent (app, event => e) = sfTrue loop
         case e.eventType is
            when sfEvtClosed =>
               RenderWindow.close (app);
            when sfEvtKeyPressed =>
               Last_Key := Translate_Scancode (e.key.scancode);
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
      return isScancodePressed (Key_Conv (K)) = sfTrue;
   end Key_Down;

   function Key_Up (K : Key) return Boolean is
   begin
      return not Key_Down (K);
   end Key_Up;

   function Translate_Key (K : Key) return sfScancode is
   begin
      return Key_Conv (K);
   end Translate_Key;

   function Translate_Scancode (S : sfScancode) return Key
   is begin
      for I in Key_Map'Range loop
         if Key_Conv (I) = S then
            return I;
         end if;
      end loop;

      return Unknown;
   end Translate_Scancode;
end Video;
