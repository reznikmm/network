--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Ada.Streams;

with League.Strings;

with Network.Addresses;

package Network.Connections is
   pragma Preelaborate;

   type Event_Listener is limited interface;
   --  Connection event listener

   type Event_Listener_Access is access all Event_Listener'Class
     with Storage_Size => 0;

   not overriding procedure Connected
     (Self   : in out Event_Listener;
      Remote : Network.Addresses.Address) is null;
   --  Called once when the connection is established.

   not overriding procedure Closed
     (Self  : in out Event_Listener;
      Error : League.Strings.Universal_String) is null;
   --  Called once a connection is closed or connect fails.

   not overriding procedure Can_Write
     (Self : in out Event_Listener) is null;
   --  Called once when it's possible to write data again.

   not overriding procedure Can_Read
     (Self : in out Event_Listener) is null;
   --  Called once when it's possible to read data again.

   type Connection is limited interface;
   --  A network connection. The connectionis created in a closed stated and
   --  stays closed until a listener is assigned to it. Then the listener gets
   --  'Connected' signal or (if error occurs) a 'Closed' signal.

   type Connection_Access is access all Connection'Class
     with Storage_Size => 0;

   not overriding function Is_Closed
     (Self : Connection) return Boolean is abstract;
   --  The connection isn't open yet or already closed.

   not overriding procedure Set_Listener
     (Self     : in out Connection;
      Listener : Event_Listener_Access) is abstract
        with Pre'Class => Self.Is_Closed;
   --  Assign event listener

   not overriding procedure Write
     (Self : in out Connection;
      Data : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset) is abstract
        with Pre'Class => not Self.Is_Closed;
   --  Do nothing if Self.Is_Closed. Last is set to index of the last
   --  element to be written. If Last < Data'Last it means incomplete
   --  operation, Standard_Input_Available notification will be called once
   --  operation can be continued. Application is responsible to call this
   --  subprogram again for remaining data.

   not overriding procedure Read
     (Self : in out Connection;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset) is abstract
        with Pre'Class => not Self.Is_Closed;

   not overriding procedure Close (Self : in out Connection) is abstract
     with Pre'Class => not Self.Is_Closed,
          Post'Class => Self.Is_Closed;
   --  Close connection

end Network.Connections;
