--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Ada.Unchecked_Deallocation;

package body Network.Connections is

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Connection) is
   begin
      if Self.Object /= null then
         Self.Object.Reference;
      end if;
   end Adjust;

   -----------
   -- Close --
   -----------

   overriding procedure Close (Self : in out Connection) is
   begin
      if Self.Object /= null then
         Self.Object.Close;
      end if;
   end Close;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Connection) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Network.Abstract_Connections.Abstract_Connection'Class,
         Connection_Access);

   begin
      if Self.Object /= null and then Self.Object.Dereference then
         Free (Self.Object);
      end if;
   end Finalize;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Self : in out Connection;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset) is
   begin
      if Self.Object = null then
         Last := Ada.Streams.Stream_Element_Offset'Pred (Data'First);
      else
         Self.Object.Read (Data, Last);
      end if;
   end Read;

   ------------
   -- Remote --
   ------------

   not overriding function Remote (Self : Connection)
      return Network.Addresses.Address is (Self.Object.Remote);

   ------------------------
   -- Set_Input_Listener --
   ------------------------

   overriding procedure Set_Input_Listener
     (Self     : in out Connection;
      Listener : Network.Streams.Input_Listener_Access) is
   begin
      if Self.Object /= null then
         Self.Object.Set_Input_Listener (Listener);
      end if;
   end Set_Input_Listener;

   ------------------
   -- Set_Listener --
   ------------------

   procedure Set_Listener
     (Self  : in out Connection'Class;
      Value : Listener_Access) is
   begin
      Self.Set_Input_Listener
        (Network.Streams.Input_Listener_Access (Value));

      Self.Set_Output_Listener
        (Network.Streams.Output_Listener_Access (Value));
   end Set_Listener;

   -------------------------
   -- Set_Output_Listener --
   -------------------------

   overriding procedure Set_Output_Listener
     (Self     : in out Connection;
      Listener : Network.Streams.Output_Listener_Access) is
   begin
      if Self.Object /= null then
         Self.Object.Set_Output_Listener (Listener);
      end if;
   end Set_Output_Listener;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self : in out Connection;
      Data : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset) is
   begin
      if Self.Object = null then
         Last := Ada.Streams.Stream_Element_Offset'Pred (Data'First);
      else
         Self.Object.Write (Data, Last);
      end if;
   end Write;

end Network.Connections;
