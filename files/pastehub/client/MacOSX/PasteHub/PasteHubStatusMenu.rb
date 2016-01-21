#
# PasteHubStatusMenu.rb - PasteHub's MacOS X client application.
#  
#   Copyright (c) 2009-2011  Kiyoka Nishiyama  <kiyoka@sumibi.org>
#   
#   Redistribution and use in source and binary forms, with or without
#   modification, are permitted provided that the following conditions
#   are met:
#   
#   1. Redistributions of source code must retain the above copyright
#      notice, this list of conditions and the following disclaimer.
#  
#   2. Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in the
#      documentation and/or other materials provided with the distribution.
#  
#   3. Neither the name of the authors nor the names of its contributors
#      may be used to endorse or promote products derived from this
#      software without specific prior written permission.
#  
#   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
#   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
#   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
#   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
#   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
#   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
#   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
#   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#  
#
class PasteHubStatusMenu < NSMenu
    attr_accessor :status_bar_item

    def awakeFromNib
        self.status_bar_item = NSStatusBar.systemStatusBar.statusItemWithLength(NSVariableStatusItemLength)
    
        setStatusIcon(:normal)

        # regist observer for "respond_to_status_change"
        NSNotificationCenter.defaultCenter.addObserver(self,
                                                       selector:"respond_to_change_status:",
                                                       name:"change_status", object:nil)
    end

    def setStatusIcon(sym)
        case sym
        when :offline
            image = NSImage.imageNamed 'pastehub_statusbar_normal.png'
        when :online
            image = NSImage.imageNamed 'pastehub_statusbar_checked.png'
        when :one
            image = NSImage.imageNamed 'pastehub_statusbar_1.png'
        when :two
            image = NSImage.imageNamed 'pastehub_statusbar_2.png'
        when :three
            image = NSImage.imageNamed 'pastehub_statusbar_3.png'
        when :threeplus
            image = NSImage.imageNamed 'pastehub_statusbar_3plus.png'
        when :ng
            image = NSImage.imageNamed 'pastehub_statusbar_ng.png'
        end
        self.status_bar_item.setImage image
        self.status_bar_item.setHighlightMode true
        self.status_bar_item.setMenu self
    end
    
    def respond_to_change_status(notification)
        p "respond_to_change_status"
        setStatusIcon(notification.userInfo[:status])
    end

end
