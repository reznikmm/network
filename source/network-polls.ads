--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Ada.Finalization;
with Interfaces.C;

package Network.Polls is
   pragma Preelaborate;

   type Listener is limited interface;
   type Listener_Access is access all Listener'Class with Storage_Size => 0;

   type Event is (Input, Output, Error, Close);
   type Event_Set is array (Event) of Boolean with Pack;
   subtype FD is Interfaces.C.int;

   not overriding procedure On_Event
     (Self   : in out Listener;
      Events : Event_Set) is null;

   type Poll is tagged limited private;
   type Poll_Access is access all Poll'Class with Storage_Size => 0;

   function Is_Initialized (Self : Poll'Class) return Boolean;

   procedure Initialize (Self : out Poll'Class)
     with Pre => not Self.Is_Initialized,
          Post => Self.Is_Initialized;

   procedure Watch
     (Self     : in out Poll'Class;
      Value    : FD;
      Events   : Event_Set;
      Listener : Listener_Access)
     with Pre => Self.Is_Initialized;

   procedure Change_Watch
     (Self     : in out Poll'Class;
      Events   : Event_Set;
      Value    : FD;
      Listener : Listener_Access)
     with Pre => Self.Is_Initialized;

   procedure Wait
     (Self    : in out Poll'Class;
      Timeout : Duration)
     with Pre => Self.Is_Initialized;

private

   type Poll is new Ada.Finalization.Limited_Controlled with record
      Count    : Natural := 0;
      Internal : FD := 0;
   end record;

   overriding procedure Finalize (Self : in out Poll);

end Network.Polls;
