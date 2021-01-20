--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Ada.Streams;

with League.Strings;

package Network.Streams is
   pragma Preelaborate;

   type Event_Listener is limited interface;
   --  Stream event listener

   type Event_Listener_Access is access all Event_Listener'Class
     with Storage_Size => 0;

   not overriding procedure Closed
     (Self  : in out Event_Listener;
      Error : League.Strings.Universal_String) is null;
   --  Called once a Stream is closed.

   not overriding procedure Can_Write
     (Self : in out Event_Listener) is null;
   --  Called once when it's possible to write data again.

   not overriding procedure Can_Read
     (Self : in out Event_Listener) is null;
   --  Called once when it's possible to read data again.

   type Stream is limited interface;
   --  A stream of Stream_Elements (bytes). Initially the stream doesn't have a
   --  listener. It's possible to read and write data after setting a listener
   --  to it until the stream is closed.
   --
   --  Shall we split the Stream to read and write streams???

   type Stream_Access is access all Stream'Class
     with Storage_Size => 0;
   --  A stream access

   not overriding function Has_Listener
     (Self : Stream) return Boolean is abstract;
   --  Return True if Set_Listener was called with not null value.

   not overriding function Is_Closed
     (Self : Stream) return Boolean is abstract;
   --  The Stream isn't open yet or already closed.

   not overriding procedure Set_Listener
     (Self     : in out Stream;
      Listener : Event_Listener_Access) is abstract;
   --  Assign event listener

   not overriding procedure Write
     (Self : in out Stream;
      Data : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset) is abstract
        with Pre'Class => Self.Has_Listener and not Self.Is_Closed;
   --  Last is set to index of the last element to be written. If Last <
   --  Data'Last it means incomplete operation, Can_Write notification will
   --  be called once operation can be continued. Application is responsible
   --  to call this subprogram again for remaining data. Do nothing if
   --  Self.Is_Closed.

   not overriding procedure Read
     (Self : in out Stream;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset) is abstract
        with Pre'Class => Self.Has_Listener and not Self.Is_Closed;

   not overriding procedure Close (Self : in out Stream) is abstract
     with Pre'Class => not Self.Is_Closed,
          Post'Class => Self.Is_Closed;
   --  Close the Stream

end Network.Streams;
