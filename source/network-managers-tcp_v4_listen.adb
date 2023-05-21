--  SPDX-FileCopyrightText: 2022 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Ada.Exceptions;
with Interfaces.C;

with Network.Managers.TCP_V4_In;

package body Network.Managers.TCP_V4_Listen is

   --------------
   -- On_Event --
   --------------

   overriding procedure On_Event
     (Self   : in out Listen_Socket;
      Events : Network.Polls.Event_Set)
   is
      Remote : GNAT.Sockets.Sock_Addr_Type;
      Image  : League.Strings.Universal_String;
      Req    : GNAT.Sockets.Request_Type :=
        (GNAT.Sockets.Non_Blocking_IO, Enabled => True);
      Socket : constant TCP_V4_In.In_Socket_Access :=
        new TCP_V4_In.In_Socket (Self.Poll);
   begin
      begin
         GNAT.Sockets.Accept_Socket (Self.Internal, Socket.Internal, Remote);
      exception
         when E : GNAT.Sockets.Socket_Error =>
            Socket.Error := League.Strings.From_UTF_8_String
              (Ada.Exceptions.Exception_Message (E));
      end;

      Image.Append ("/ip4/");
      Image.Append
        (League.Strings.From_UTF_8_String (GNAT.Sockets.Image (Remote.Addr)));
      Image.Append ("/tcp/");

      declare
         Port : constant Wide_Wide_String := Remote.Port'Wide_Wide_Image;
      begin
         Image.Append (Port (2 .. Port'Last));

         Socket.Remote := Network.Addresses.To_Address (Image);
      end;

      GNAT.Sockets.Control_Socket (Socket.Internal, Req);

      Self.Listener.Connected
        (Connection => Connections.Connection_Access (Socket),
         Remote     => Socket.Remote);

      pragma Assert (Socket.Listener.Assigned);
      Socket.Events := (others => True);

      Self.Poll.Watch
        (Interfaces.C.int (GNAT.Sockets.To_C (Socket.Internal)),
         Events   => Socket.Events,
         Listener => Socket.all'Access);
   end On_Event;

end Network.Managers.TCP_V4_Listen;
