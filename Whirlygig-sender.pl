#!/usr/bin/perl

#**************************************************************************
#*   Whirly-GIG (Graphical Information Gyro)                              *
#*                                                                        *
#*   Copyright (C) 2002 Craig Shelley                                     *
#*   This program is free software; you can redistribute it and/or modify *
#*   it under the terms of the GNU General Public License as published by *
#*   the Free Software Foundation; either version 2, or (at your option)  *
#*   any later version.                                                   *
#*                                                                        *
#*   This program is distributed in the hope that it will be useful,      *
#*   but WITHOUT ANY WARRANTY; without even the implied warranty of       *
#*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *
#*   GNU General Public License for more details.                         *
#*                                                                        *
#*   You should have received a copy of the GNU General Public License    *
#*   along with this program; if not, write to the Free Software          *
#*   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.            *
#*                                                                        *
#**************************************************************************


$buffsize=51;


open(TTYS,">/dev/ttyS1");
select(TTYS);$|=1;select(STDOUT);
$buffer="";
$tosend="";
$texttime=0.30;
$scrolltime=3.5;

if ($ARGV[0] ne "-")
{
	print "Turning on\n";
	print TTYS " ";
	sleep 30;
	print "Clearing Display\n";
	print TTYS " " x $buffsize . "\0";
	sleep 15;
}

while(1)
{
	$line = <STDIN>;
	$line =~ s/\n/\0/;
	$truncate=$buffsize - 2;
	$line =~ s/(.{$truncate})/$1\0/g;
	if ($line eq "")
	{
		$line= " " x $buffsize;
		if ($buffer =~ /\ {$buffsize}/) {
			if ($ARGV[0] ne "-")
			{
				print "Turning off in 5 seconds\n";
				sleep 5;
				print TTYS "\xFF";
			}
			exit;
		}
	}
	$tosend = $tosend . $line;
	if (length($buffer) + length($tosend) > $buffsize)
	{
		$send=$buffsize - length($buffer); 
		if (substr($tosend,$send-1,1) eq "\0")
		{
			print "Extra Space inserted to rotate buffer\n";
			substr($tosend,$send-1,1) = " \0";
		}
		print TTYS substr($tosend,0,$send);
		
		$senddisplay= substr($tosend,0,$send);
		$senddisplay =~ s/\0/\x7f/g;
		print "SEND\t<$senddisplay>\n";
		
		$buffer = $buffer . substr($tosend,0,$send);
		$displaytext = $buffer;
		$displaytext =~ s/\0.*//;
		print "DISPLAY\t<$displaytext>\n\n";
		
		select (undef,undef,undef,((length($displaytext) * $texttime)) + $scrolltime);
		$buffer =~ s/.*?\0//;
		$tosend=substr($tosend,$send);
	}
	else
	{
		print TTYS $tosend;
		$senddisplay=$tosend;
		$senddisplay =~ s/\0/\x7f/g;
		print "SEND\t<$senddisplay>\n";
		
		$buffer = $buffer . $tosend;
		if (length($buffer) == $buffsize)
		{
			$displaytext = $buffer;
			$displaytext =~ s/\0.*//;
			print "DISPLAY\t<$displaytext>\n\n";
			
			select (undef,undef,undef,((length($displaytext) * $texttime)) + $scrolltime);
			$buffer =~ s/.*?\0//;
		}
		$tosend="";
	}
	$bufferdisplay=$buffer;
	$bufferdisplay =~ s/\0/\x7f/g;
	print "BUFFER\t<$bufferdisplay>\n";
}
