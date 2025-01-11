with Sf.Window;
with Sf.Window.Event;
with Sf.Graphics.Color;

package body Video is
   procedure Clear_Screen is
   begin
      for X in 0 .. sfUint32 (Width) - 1 loop
         for Y in 0 .. sfUint32 (Height) - 1 loop
            Image.setPixel (Pixels, X, Y, Color.sfBlack);
         end loop;
      end loop;
   end Clear_Screen;

   procedure Initialize is
   begin
      Sprite.setTexture (Pixels_Sprite, Pixels_Texture);
      Sprite.setPosition (Pixels_Sprite, (Float (0), Float (0)));
      Sprite.setScale (Pixels_Sprite, (Float (Scale), Float (Scale)));
      RenderWindow.setFramerateLimit (app, 60);
   end Initialize;

   function Is_Running return Boolean is
   begin
      return RenderWindow.isOpen (app) = sfTrue;
   end Is_Running;

   procedure Display is
      use Sf.Window.Event;

      e : sfEvent;
   begin
      while RenderWindow.pollEvent (app, event => e) = sfTrue loop
         if e.eventType = sfEvtClosed then
            RenderWindow.close (app);
         end if;
      end loop;

      RenderWindow.clear (app, Color.sfWhite);
      Texture.updateFromImage (Pixels_Texture, Pixels, 0, 0);
      RenderWindow.drawSprite (app, Pixels_Sprite);
      RenderWindow.display (app);
   end Display;

   function Toggle_Pixel (X, Y : sfUint32) return Boolean is
      use Color;

      P : constant sfColor := Image.getPixel (Pixels, X, Y);
      R : constant Boolean := P = sfWhite;
   begin
      Image.setPixel (Pixels, X, Y, (if R then sfBlack else sfWhite));
      return R;
   end Toggle_Pixel;

   procedure Finish is
   begin
      RenderWindow.close (app);
   end Finish;
end Video;
