--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Ada.Streams;

with League.Strings;

package Network.Streams is
   pragma Preelaborate;

   ------------------
   -- Input Stream --
   ------------------

   type Input_Listener is limited interface;
   --  Input stream event listener

   type Input_Listener_Access is access all Input_Listener'Class
     with Storage_Size => 0;

   not overriding procedure Closed
     (Self  : in out Input_Listener;
      Error : League.Strings.Universal_String) is null;
   --  Called once a Stream is closed.

   not overriding procedure Can_Read
     (Self : in out Input_Listener) is null;
   --  Called once when it's possible to read data again.

   type Input_Stream is limited interface;
   --  A stream of Stream_Elements (bytes). Initially the stream doesn't have a
   --  listener. It's possible to read data after setting a listener to it
   --  until the stream is closed.

   not overriding function Has_Listener
     (Self : Input_Stream) return Boolean is abstract;
   --  Return True if Set_Listener was called with not null value.

   not overriding function Is_Closed
     (Self : Input_Stream) return Boolean is abstract;
   --  The Stream isn't open yet or already closed.

   not overriding procedure Set_Input_Listener
     (Self     : in out Input_Stream;
      Listener : Input_Listener_Access) is abstract;
   --  Assign event listener

   not overriding procedure Read
     (Self : in out Input_Stream;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset) is abstract
        with Pre'Class => Self.Has_Listener and not Self.Is_Closed;
   --  Last is set to index of the last read element. If Last < Data'First it
   --  means an incomplete operation, Can_Read notification will be called once
   --  operation can be continued. It's not possible to read date before a
   --  listener assigned or after the stream is closed.

   not overriding procedure Close (Self : in out Input_Stream) is abstract
     with Pre'Class => not Self.Is_Closed,
          Post'Class => Self.Is_Closed;
   --  Close the Stream

   -------------------
   -- Output Stream --
   -------------------

   type Output_Listener is limited interface;
   --  Output Stream event listener

   type Output_Listener_Access is access all Output_Listener'Class
     with Storage_Size => 0;

   not overriding procedure Closed
     (Self  : in out Output_Listener;
      Error : League.Strings.Universal_String) is null;
   --  Called once a Stream is closed.

   not overriding procedure Can_Write
     (Self : in out Output_Listener) is null;
   --  Called once when it's possible to write data again.

   type Output_Stream is limited interface;
   --  A stream of Stream_Elements (bytes). Initially the stream doesn't have a
   --  listener. It's possible to write data after setting a listener to it
   --  until the stream is closed.

   not overriding function Has_Listener
     (Self : Output_Stream) return Boolean is abstract;
   --  Return True if Set_Listener was called with not null value.

   not overriding function Is_Closed
     (Self : Output_Stream) return Boolean is abstract;
   --  The Stream isn't open yet or already closed.

   not overriding procedure Set_Output_Listener
     (Self     : in out Output_Stream;
      Listener : Output_Listener_Access) is abstract;
   --  Assign event listener

   not overriding procedure Write
     (Self : in out Output_Stream;
      Data : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset) is abstract
        with Pre'Class => Self.Has_Listener and not Self.Is_Closed;
   --  Last is set to index of the last element to be written. If Last <
   --  Data'Last it means an incomplete operation, Can_Write notification will
   --  be called once operation can be continued. Application is responsible
   --  to call this subprogram again for remaining data. It's not possible to
   --  read date before a listener assigned or after the stream is closed.

   not overriding procedure Close (Self : in out Output_Stream) is abstract
     with Pre'Class => not Self.Is_Closed,
          Post'Class => Self.Is_Closed;
   --  Close the Stream

end Network.Streams;
