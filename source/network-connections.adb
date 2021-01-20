--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

package body Network.Connections is

   ------------------
   -- Set_Listener --
   ------------------

   procedure Set_Listener
     (Self  : in out Connection'Class;
      Value : Listener_Access)
   is
   begin
      Self.Set_Input_Listener
        (Network.Streams.Input_Listener_Access (Value));

      Self.Set_Output_Listener
        (Network.Streams.Output_Listener_Access (Value));
   end Set_Listener;

end Network.Connections;
