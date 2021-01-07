--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Ada.Unchecked_Conversion;
with Interfaces;

with League.String_Vectors;
with League.Regexps;
with League.Text_Codecs;
with League.Stream_Element_Vectors;

package body Network.Addresses is

   function "+"
     (Text : Wide_Wide_String) return League.Strings.Universal_String
      renames League.Strings.To_Universal_String;

   type Protocol_Name is
     (ip4,
      tcp,
      dccp,
      ip6,
      ip6zone,  --  rfc4007 IPv6 zone
      dns,  --  domain name resolvable to both IPv6 and IPv4 addresses
      dns4,  --  domain name resolvable only to IPv4 addresses
      dns6,  --  domain name resolvable only to IPv6 addresses
      dnsaddr,
      sctp,
      udp,
      p2p_webrtc_star,
      p2p_webrtc_direct,
      p2p_stardust,
      p2p_circuit,
      udt,
      utp,
      unix,
      p2p,  --  preferred over /ipfs
      https,
      onion,
      onion3,
      garlic64,
      garlic32,
      quic,
      ws,
      wss,
      p2p_websocket_star,
      http,
      memory  --  in memory transport for self-dialing and testing; arbitrary
     );

   for Protocol_Name'Size use 16;
   for Protocol_Name use
     (ip4                => 4,
      tcp                => 6,
      dccp               => 33,
      ip6                => 41,
      ip6zone            => 42,
      dns                => 53,
      dns4               => 54,
      dns6               => 55,
      dnsaddr            => 56,
      sctp               => 132,
      udp                => 273,
      p2p_webrtc_star    => 275,
      p2p_webrtc_direct  => 276,
      p2p_stardust       => 277,
      p2p_circuit        => 290,
      udt                => 301,
      utp                => 302,
      unix               => 400,
      --  ipfs => 421,
      p2p                => 421,
      https              => 443,
      onion              => 444,
      onion3             => 445,
      garlic64           => 446,
      garlic32           => 447,
      quic               => 460,
      ws                 => 477,
      wss                => 478,
      p2p_websocket_star => 479,
      http               => 480,
      memory             => 777);

   procedure Read_Protocol_Name
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Value  : out Protocol_Name);

   procedure Write_Protocol_Name
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Value  : Protocol_Name);

   for Protocol_Name'Read use Read_Protocol_Name;
   for Protocol_Name'Write use Write_Protocol_Name;

   ipfs : constant Protocol_Name := p2p;
   --  backwards compatibility; equivalent to /p2p

   Offset : constant array (Protocol_Name) of Positive :=
     (ip4                => 1,
      tcp                => 4,
      dccp               => 7,
      ip6                => 11,
      ip6zone            => 14,
      dns                => 21,
      dns4               => 24,
      dns6               => 28,
      dnsaddr            => 32,
      sctp               => 39,
      udp                => 43,
      p2p_webrtc_star    => 46,
      p2p_webrtc_direct  => 61,
      p2p_stardust       => 78,
      p2p_circuit        => 90,
      udt                => 101,
      utp                => 104,
      unix               => 107,
      p2p                => 111,
      https              => 114,
      onion              => 119,
      onion3             => 124,
      garlic64           => 130,
      garlic32           => 138,
      quic               => 146,
      ws                 => 150,
      wss                => 152,
      p2p_websocket_star => 155,
      http               => 173,
      memory             => 177);

   Any_Image : constant Wide_Wide_String :=
      "ip4" &
      "tcp" &
      "dccp" &
      "ip6" &
      "ip6zone" &
      "dns" &
      "dns4" &
      "dns6" &
      "dnsaddr" &
      "sctp" &
      "udp" &
      "p2p_webrtc_star" &
      "p2p_webrtc_direct" &
      "p2p_stardust" &
      "p2p_circuit" &
      "udt" &
      "utp" &
      "unix" &
      "p2p" &
      "https" &
      "onion" &
      "onion3" &
      "garlic64" &
      "garlic32" &
      "quic" &
      "ws" &
      "wss" &
      "p2p_websocket_star" &
      "http" &
      "memory";

   subtype Byte is Interfaces.Unsigned_8;
   subtype Port is Interfaces.Unsigned_16;

   function Image (Value : Byte) return Wide_Wide_String with Inline;
   function Image (Value : Port) return Wide_Wide_String with Inline;
   function Image (Name : Protocol_Name) return Wide_Wide_String with Inline;

   procedure To_Value
     (Text : Wide_Wide_String;
      Name : out Protocol_Name;
      Ok   : out Boolean) with Inline;

   IP4_Pattern_Text : constant Wide_Wide_String :=
     "^(25[0-5]|2[0-4][0-9]|[0-1]?[0-9]{1,2})"
       & "(\.(25[0-5]|2[0-4][0-9]|[0-1]?[0-9]{1,2})){3}$";

   Port_Pattern_Text : constant Wide_Wide_String := "^[0-6]?[0-9]{1,4}$";

   DNS_Pattern_Text : constant Wide_Wide_String :=
     "^[a-zA-Z0-9\-]+(\.[a-zA-Z0-9\-]+)+$";

   type IP4_Raw is array (1 .. 4) of Byte;

   -----------
   -- Image --
   -----------

   function Image (Value : Byte) return Wide_Wide_String is
      Text : constant Wide_Wide_String := Byte'Wide_Wide_Image (Value);
   begin
      return Text (2 .. Text'Last);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Value : Port) return Wide_Wide_String is
      Text : constant Wide_Wide_String := Port'Wide_Wide_Image (Value);
   begin
      return Text (2 .. Text'Last);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Name : Protocol_Name) return Wide_Wide_String is
      To : Positive := Any_Image'Last;
   begin
      if Name /= Protocol_Name'Last then
         To := Offset (Protocol_Name'Succ (Name)) - 1;
      end if;

      return Any_Image (Offset (Name) .. To);
   end Image;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid
     (Value : League.Strings.Universal_String) return Boolean
   is
      IP4_Pattern : constant League.Regexps.Regexp_Pattern :=
        League.Regexps.Compile (+IP4_Pattern_Text);
      Port_Pattern : constant League.Regexps.Regexp_Pattern :=
        League.Regexps.Compile (+Port_Pattern_Text);
      DNS_Pattern  : constant League.Regexps.Regexp_Pattern :=
        League.Regexps.Compile (+DNS_Pattern_Text);

      Has_Proto : Boolean := False;
      Proto     : Protocol_Name := Protocol_Name'First;
      List      : constant League.String_Vectors.Universal_String_Vector :=
        Value.Split ('/');
   begin
      if List.Length < 2 or else not List (1).Is_Empty then
         return False;
      end if;

      for J in 2 .. List.Length loop
         declare
            Item : constant League.Strings.Universal_String := List (J);
         begin
            if Has_Proto then
               Has_Proto := False;

               case Proto is
                  when ip4 =>
                     if not IP4_Pattern.Find_Match (Item).Is_Matched then
                        return False;
                     end if;
                  when tcp | udp | dccp | sctp =>
                     if not Port_Pattern.Find_Match (Item).Is_Matched then
                        return False;
                     end if;
                  when dns | dns4 | dns6 | dnsaddr =>
                     if not DNS_Pattern.Find_Match (Item).Is_Matched then
                        return False;
                     end if;
                  when others =>
                     null;
               end case;
            else
               To_Value (Item.To_Wide_Wide_String, Proto, Has_Proto);

               if not Has_Proto then
                  return False;
               end if;
            end if;
         end;
      end loop;

      return not Has_Proto;
   end Is_Valid;

   ----------
   -- Read --
   ----------

   procedure Read
     (Self  : access Ada.Streams.Root_Stream_Type'Class;
      Value : out Ada.Streams.Stream_Element_Count)
   is
      use type Interfaces.Unsigned_64;
      use type Byte;
      Shift  : Natural := 0;
      Item   : Byte;
      Result : Interfaces.Unsigned_64;
   begin
      Result := 0;

      loop
         Byte'Read (Self, Item);
         Result := Result + Interfaces.Shift_Left
           (Interfaces.Unsigned_64 (Item and 127),
            Shift);
         Shift := Shift + 7;
         exit when Item < 128;
      end loop;

      Value := Ada.Streams.Stream_Element_Count (Result);
   end Read;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Self : in out Simple_Stream;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
      use type Ada.Streams.Stream_Element_Offset;
      To : constant Ada.Streams.Stream_Element_Offset :=
        Ada.Streams.Stream_Element_Offset'Min
          (Self.Last + Item'Length, Self.Size);
   begin
      Last := Item'First + To - Self.Last - 1;
      Item (Item'First .. Last) := Self.Data (Self.Last + 1 .. To);
      Self.Last := To;
   end Read;

   ------------------------
   -- Read_Protocol_Name --
   ------------------------

   procedure Read_Protocol_Name
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Value  : out Protocol_Name)
   is
      Size : constant := Protocol_Name'Size;

      type Raw_Protocol_Name is mod 2 ** Protocol_Name'Size
        with Size => Size;

      function Convert is new Ada.Unchecked_Conversion
        (Raw_Protocol_Name, Protocol_Name);

      Raw : Ada.Streams.Stream_Element_Count;
   begin
      Read (Stream, Raw);
      Value := Convert (Raw_Protocol_Name (Raw));
   end Read_Protocol_Name;

   ----------------
   -- To_Address --
   ----------------

   function To_Address
     (Value : League.Strings.Universal_String) return Address is
   begin
      return (Value => Value);
   end To_Address;

   ----------------
   -- To_Address --
   ----------------

   function To_Address
     (Value : Ada.Streams.Stream_Element_Array) return Address
   is
      use type Ada.Streams.Stream_Element_Count;

      UTF : constant League.Text_Codecs.Text_Codec :=
        League.Text_Codecs.Codec (+"utf-8");

      S : aliased Simple_Stream :=
        (Ada.Streams.Root_Stream_Type with
           Size => Value'Length,
           Last => 0,
           Data => Value);
      Result : League.Strings.Universal_String;
      Proto  : Protocol_Name;
      Length : Ada.Streams.Stream_Element_Count;
   begin
      while S.Last < S.Size loop
         Protocol_Name'Read (S'Access, Proto);
         Result.Append ('/');
         Result.Append (Image (Proto));

         case Proto is
            when ip4 =>
               declare
                  Addr : IP4_Raw;
               begin
                  Result.Append ('/');
                  IP4_Raw'Read (S'Access, Addr);
                  Result.Append (Image (Addr (1)));

                  for X in 2 .. 4 loop
                     Result.Append ('.');
                     Result.Append (Image (Addr (X)));
                  end loop;
               end;
            when tcp | udp | dccp | sctp =>
               declare
                  use type Port;
                  High, Low : Byte;
                  Value : Port;
               begin
                  Result.Append ('/');
                  Byte'Read (S'Access, High);
                  Byte'Read (S'Access, Low);
                  Value := Port (High) * 256 + Port (Low);
                  Result.Append (Image (Value));
               end;
            when dns | dns4 | dns6 | dnsaddr =>
               Read (S'Access, Length);
               declare
                  Buffer : Ada.Streams.Stream_Element_Array (1 .. Length);
                  Text   : League.Strings.Universal_String;
               begin
                  Ada.Streams.Stream_Element_Array'Read (S'Access, Buffer);
                  Text := UTF.Decode (Buffer);
                  Result.Append ('/');
                  Result.Append (Text);
               end;
            when others =>
               null;
         end case;
      end loop;

      return (Value => Result);
   end To_Address;

   -----------------------------
   -- To_Stream_Element_Array --
   -----------------------------

   function To_Stream_Element_Array
     (Self : Address) return Ada.Streams.Stream_Element_Array
   is
      Max : constant Natural := Self.Value.Length;
      S   : aliased Simple_Stream (Ada.Streams.Stream_Element_Count (Max));
      UTF : constant League.Text_Codecs.Text_Codec :=
        League.Text_Codecs.Codec (+"utf-8");

      Has_Proto : Boolean := False;
      Proto     : Protocol_Name := Protocol_Name'First;
      List      : constant League.String_Vectors.Universal_String_Vector :=
        Self.Value.Split ('/');
   begin
      for J in 2 .. List.Length loop
         declare
            Item : constant League.Strings.Universal_String := List (J);
         begin
            if Has_Proto then
               Has_Proto := False;

               case Proto is
                  when ip4 =>
                     declare
                        List : constant
                          League.String_Vectors.Universal_String_Vector :=
                            Item.Split ('.');
                        Value : Byte;
                     begin

                        for J in 1 .. List.Length loop
                           Value := Byte'Wide_Wide_Value
                             (List (J).To_Wide_Wide_String);

                           Byte'Write (S'Access, Value);
                        end loop;
                     end;

                  when tcp | udp | dccp | sctp =>
                     declare
                        use type Port;
                        Value : constant Port := Port'Wide_Wide_Value
                          (Item.To_Wide_Wide_String);
                        High, Low : Byte;
                     begin
                        Low := Byte'Mod (Value);
                        High := Byte (Value / 256);
                        Byte'Write (S'Access, High);
                        Byte'Write (S'Access, Low);
                     end;

                  when dns | dns4 | dns6 | dnsaddr | p2p =>
                     declare
                        Image : constant
                          League.Stream_Element_Vectors.Stream_Element_Vector
                            := UTF.Encode (Item);
                     begin
                        Write (S'Access, Image.Length);
                        S.Write (Image.To_Stream_Element_Array);
                     end;

                  when others =>
                     null;
               end case;
            else
               To_Value (Item.To_Wide_Wide_String, Proto, Has_Proto);
               Protocol_Name'Write (S'Access, Proto);
            end if;
         end;
      end loop;

      return S.Data (1 .. S.Last);
   end To_Stream_Element_Array;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Self : Address) return League.Strings.Universal_String is
   begin
      return Self.Value;
   end To_String;

   --------------
   -- To_Value --
   --------------

   procedure To_Value
     (Text : Wide_Wide_String;
      Name : out Protocol_Name;
      Ok   : out Boolean) is
   begin
      Ok := False;

      if Text = "ipfs" then
         Name := ipfs;
         Ok := True;
         return;
      end if;

      for P in Protocol_Name loop
         if Image (P) = Text then
            Name := P;
            Ok := True;
            exit;
         end if;
      end loop;
   end To_Value;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self : in out Simple_Stream;
      Item : Ada.Streams.Stream_Element_Array)
   is
      use type Ada.Streams.Stream_Element_Offset;
   begin
      Self.Data (Self.Last + 1 .. Self.Last + Item'Length) := Item;
      Self.Last := Self.Last + Item'Length;
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (Self  : access Ada.Streams.Root_Stream_Type'Class;
      Value : Ada.Streams.Stream_Element_Count)
   is
      use type Byte;
      use type Ada.Streams.Stream_Element_Count;
      Next : Ada.Streams.Stream_Element_Count := Value;
      Item : Byte;
   begin
      while Next > 127 loop
         Item := Byte'Mod (Next) or 128;
         Byte'Write (Self, Item);
         Next := Next / 128;
      end loop;

      Item := Byte'Mod (Next);
      Byte'Write (Self, Item);
   end Write;

   -------------------------
   -- Write_Protocol_Name --
   -------------------------

   procedure Write_Protocol_Name
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Value  : Protocol_Name)
   is
      Size : constant := Protocol_Name'Size;

      type Raw_Protocol_Name is mod 2 ** Protocol_Name'Size
        with Size => Size;

      function Convert is new Ada.Unchecked_Conversion
        (Protocol_Name, Raw_Protocol_Name);

      Raw  : constant Raw_Protocol_Name := Convert (Value);
   begin
      Write (Stream, Ada.Streams.Stream_Element_Count (Raw));
   end Write_Protocol_Name;

end Network.Addresses;
