--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Ada.Streams;

with League.Strings;

package Network.Addresses is
   pragma Preelaborate;

   type Address is private;
   --  Composable and future-proof network addresses

   function To_String
     (Self : Address) return League.Strings.Universal_String;
   --  Return textual representation of the multiaddr. Example:
   --  "/ip4/127.0.0.1/tcp/8080". See Multiaddr for details.

   function To_Stream_Element_Array
     (Self : Address) return Ada.Streams.Stream_Element_Array;
   --  Return binary representation of the multiaddr

   function To_Address
     (Value : League.Strings.Universal_String) return Address
       with Pre => Is_Valid (Value);
   --  Construct multiaddr from the string

   function To_Address
     (Value : Ada.Streams.Stream_Element_Array) return Address
       with Pre => Is_Valid (Value);
   --  Construct multiaddr from bytes

   function Is_Valid
     (Value : League.Strings.Universal_String) return Boolean;
   --  Check is given string represents a multiaddr

   function Is_Valid
     (Ignore : Ada.Streams.Stream_Element_Array) return Boolean is (True);
   --  TBD

   type Address_Array is array (Positive range <>) of Address;

private

   type Address is record
      Value : League.Strings.Universal_String;
   end record;

   type Simple_Stream (Size : Ada.Streams.Stream_Element_Count) is
     new Ada.Streams.Root_Stream_Type with
   record
      Last : Ada.Streams.Stream_Element_Count := 0;
      Data : Ada.Streams.Stream_Element_Array (1 .. Size);
   end record;

   overriding procedure Read
     (Self : in out Simple_Stream;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);

   overriding procedure Write
     (Self : in out Simple_Stream;
      Item : Ada.Streams.Stream_Element_Array);

   procedure Write
     (Self  : access Ada.Streams.Root_Stream_Type'Class;
      Value : Ada.Streams.Stream_Element_Count);

   procedure Read
     (Self  : access Ada.Streams.Root_Stream_Type'Class;
      Value : out Ada.Streams.Stream_Element_Count);

end Network.Addresses;
