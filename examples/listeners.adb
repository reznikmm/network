--  Copyright (c) 2021 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Text_IO;
with Ada.Streams;

with League.String_Vectors;
with League.Characters.Latin;
with League.Text_Codecs;
with League.Stream_Element_Vectors;

package body Listeners is

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   ---------------
   -- Connected --
   ---------------

   overriding procedure Connected
     (Self   : in out Listener;
      Remote : Network.Addresses.Address)
   is
      pragma Unreferenced (Remote);
      List : League.String_Vectors.Universal_String_Vector;
      CRLF : League.Strings.Universal_String;
   begin
      Ada.Text_IO.Put_Line ("Connected");
      CRLF.Append (League.Characters.Latin.Carriage_Return);
      CRLF.Append (League.Characters.Latin.Line_Feed);
      List.Append (+"GET / HTTP/1.1");
      List.Append (+"Host: www.ada-ru.org");
      List.Append (+"");
      List.Append (+"");

      declare
         Last : Ada.Streams.Stream_Element_Count;
         Data : constant Ada.Streams.Stream_Element_Array :=
           League.Text_Codecs.Codec_For_Application_Locale.Encode
             (List.Join (CRLF)).To_Stream_Element_Array;
      begin
         Self.Connect.Write (Data, Last);
         Ada.Text_IO.Put_Line (Last'Image);
         Ada.Streams.Stream_IO.Create (Self.Output, Name => "/tmp/aaa.bin");
      end;
   end Connected;

   ------------
   -- Closed --
   ------------

   overriding procedure Closed
     (Self  : in out Listener;
      Error : League.Strings.Universal_String)
   is
      pragma Unreferenced (Self);
   begin
      Ada.Text_IO.Put_Line ("Closed: " & Error.To_UTF_8_String);
   end Closed;

   ---------------
   -- Can_Write --
   ---------------

   overriding procedure Can_Write (Self : in out Listener) is
      pragma Unreferenced (Self);
   begin
      Ada.Text_IO.Put_Line ("Can_Write");
   end Can_Write;

   --------------
   -- Can_Read --
   --------------

   overriding procedure Can_Read (Self : in out Listener) is
      use type Ada.Streams.Stream_Element_Count;

      Count : Natural := 0;
      Data : Ada.Streams.Stream_Element_Array (1 .. 512);
      Last : Ada.Streams.Stream_Element_Count;
   begin
      Ada.Text_IO.Put_Line ("Can_Read");

      loop
         Self.Connect.Read (Data, Last);
         Ada.Streams.Stream_IO.Write (Self.Output, Data (1 .. Last));
         exit when Last < Data'First;
         Count := Count + 1;
      end loop;

      Ada.Text_IO.Put_Line (Count'Image & Last'Image);
   end Can_Read;

end Listeners;
