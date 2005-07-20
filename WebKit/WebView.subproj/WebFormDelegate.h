/*
 * Copyright (C) 2005 Apple Computer, Inc.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1.  Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer. 
 * 2.  Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution. 
 * 3.  Neither the name of Apple Computer, Inc. ("Apple") nor the names of
 *     its contributors may be used to endorse or promote products derived
 *     from this software without specific prior written permission. 
 *
 * THIS SOFTWARE IS PROVIDED BY APPLE AND ITS CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL APPLE OR ITS CONTRIBUTORS BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/*
        WebFormDelegate.h
        Copyright 2003, Apple Computer, Inc.
*/

#import <AppKit/AppKit.h>

@class DOMElement;
@class WebFrame;

/*!
    @protocol  WebFormSubmissionListener
    @discussion .
*/
@protocol WebFormSubmissionListener <NSObject>
- (void)continue;
@end

/*!
    @protocol  WebFormDelegate
    @discussion .
*/
@protocol WebFormDelegate <NSObject>

// Various methods send by controls that edit text to their delegates, which are all
// analogous to similar methods in AppKit/NSControl.h.
// These methods are forwarded from widgets used in forms to the WebFormDelegate.

- (void)controlTextDidBeginEditing:(NSNotification *)obj inFrame:(WebFrame *)frame;
- (void)controlTextDidEndEditing:(NSNotification *)obj inFrame:(WebFrame *)frame;
- (void)controlTextDidChange:(NSNotification *)obj inFrame:(WebFrame *)frame;
- (void)textDidChange:(NSNotification *)obj inFrame:(WebFrame *)frame;

- (BOOL)control:(NSControl *)control textShouldBeginEditing:(NSText *)fieldEditor inFrame:(WebFrame *)frame;
- (BOOL)control:(NSControl *)control textShouldEndEditing:(NSText *)fieldEditor inFrame:(WebFrame *)frame;

- (BOOL)control:(NSControl *)control textView:(NSTextView *)textView doCommandBySelector:(SEL)commandSelector inFrame:(WebFrame *)frame;
- (BOOL)control:(NSControl *)control textView:(NSTextView *)textView shouldHandleEvent:(NSEvent *)event inFrame:(WebFrame *)frame;

// Sent when a form is just about to be submitted (before the load is started)
// listener must be sent continue when the delegate is done.
- (void)frame:(WebFrame *)frame sourceFrame:(WebFrame *)sourceFrame willSubmitForm:(DOMElement *)form withValues:(NSDictionary *)values submissionListener:(id <WebFormSubmissionListener>)listener;
@end

/*!
    @class WebFormDelegate
    @discussion The WebFormDelegate class responds to all WebFormDelegate protocol
    methods by doing nothing. It's provided for the convenience of clients who only want
    to implement some of the above methods and ignore others.
*/
@interface WebFormDelegate : NSObject <WebFormDelegate>
{
}
@end

