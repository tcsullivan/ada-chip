with Sf;
with Sf.Graphics;
with Sf.Graphics.Image;
with Sf.Graphics.RenderWindow;
with Sf.Graphics.Sprite;
with Sf.Graphics.Texture;

package Video is
   use Sf;
   use Sf.Graphics;

   type Key is range 0 .. 15;
   type Key_Map is array (Key'Range) of Boolean;

   Width  : constant := 64;
   Height : constant := 32;
   Scale  : constant := 10;
   Title  : constant String := "Ada-Chip";

   procedure Clear_Screen;
   procedure Initialize;
   procedure Display;
   procedure Finish;

   function Key_Down (K : Key) return Boolean;
   function Key_Up (K : Key) return Boolean;
   function Is_Running return Boolean;
   function Toggle_Pixel (X, Y : sfUint32) return Boolean;

private
   Keys           : Key_Map;
   Pixels         : constant sfImage_Ptr   := Image.create (Width, Height);
   Pixels_Sprite  : constant sfSprite_Ptr  := Sprite.create;
   Pixels_Texture : constant sfTexture_Ptr :=
      Texture.createFromImage (Pixels);

   app : constant sfRenderWindow_Ptr := RenderWindow.create
      ((Width * Scale, Height * Scale, 32), Title);
end Video;
