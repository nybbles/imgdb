dojo.provide("imgdb-web.widget.TagEntryDisplayBox");
dojo.require("dijit.InlineEditBox");
dojo.require("dijit.form.TextBox");
dojo.require("dijit._Templated");

dojo.declare
(
 "imgdb-web.widget.TagEntryDisplayBox",
 [dijit._Widget, dijit._Templated],
 {
     entrytext: "Add tags",
     templatePath: dojo.moduleUrl("imgdb-web.widget",
                                  "templates/TagEntryDisplayBox.htm"),
     templateString: "",
     widgetsInTemplate: true,
     taglist: [],
     imgid: "",
     postCreate: function()
     {
       if (this.imgid == "")
       {
         throw("imgid not set");
       }

       this.tagentrybox.displayNode.style.color = "#777777";

       this._getTags();
     },
     _getTags: function()
     {
       var tagdata = {imgid : this.imgid};
       var widget = this;
       dojo.rawXhrPost({
             url: "/get-img-tags",
             handleAs: "json-comment-filtered",
             postData: dojo.toJson(tagdata),
             timeout: 1000,
             load: function(response, ioargs)
             {
               var tagstodelete = widget.taglist.slice(0);

               dojo.forEach
                 (tagstodelete,
                  function(tag)
                  {
                    widget._deleteTagDOMNode(tag);
                  },
                  this);

               dojo.forEach
                 // (response.imgid,
                 (response.taglist,
                  function(tag)
                  {
                    widget._addTagDOMNode(tag);
                  },
                  this);
               
               widget.tagentrybox.setValue("");
             },
             error: function(response, ioargs)
             {
               alert(ioargs.xhr.status + ": " + response);
             }
       });
     },
     _addTagsEventHandler: function(rawtaglist)
     {
       var splittaglist = [];
       rawtaglist = rawtaglist.split(/&quot;/);

       var i = 0;
       for (i = 0; i < rawtaglist.length; ++i)
       {
         if ((/^\s*$/).test(rawtaglist[i]))
         {
           continue;
         }

         if (i % 2 == 0)
         {
           var spaceseperatedtags = rawtaglist[i].split(/\s+/);

           var j = 0;
           for (j = 0; j < spaceseperatedtags.length; ++j)
           {
             if ((/^\s*$/).test(spaceseperatedtags[j]))
             {
               continue;
             }

             splittaglist[splittaglist.length] = spaceseperatedtags[j];
           }
         }
         else
         {
           splittaglist[splittaglist.length] = rawtaglist[i];
         }
       }

       this._addTags(splittaglist.reverse());
     },
     _addTags: function(taglist)
     {
       var tagdata = {imgid : this.imgid, taglist : taglist};
       var widget = this;
       dojo.rawXhrPost({
             url: "/add-img-tags",
             handleAs: "json-comment-filtered",
             postData: dojo.toJson(tagdata),
             timeout: 1000,
             load: function(response, ioargs)
             {
               var tagstodelete = widget.taglist.slice(0);

               dojo.forEach
                 (tagstodelete,
                  function(tag)
                  {
                    widget._deleteTagDOMNode(tag);
                  },
                  this);

               dojo.forEach
                 // (response.imgid,
                 (response.taglist,
                  function(tag)
                  {
                    widget._addTagDOMNode(tag);
                  },
                  this);
               
               widget.tagentrybox.setValue("");
             },
             error: function(response, ioargs)
             {
               alert(ioargs.xhr.status + ": " + response);
             }
       });
     },
     _addTagDOMNode: function(tag)
     {
       if (!this._doesTagExist(tag))
       {
         var newTagDiv = document.createElement("div");
         newTagDiv.className = "tag";
         newTagDiv.tag = tag;

         var newTag = document.createElement("a");
         newTag.className = "tagname";
         newTag.innerHTML = tag;

         var newTagDelete = document.createElement("a");
         newTagDelete.className = "tagdelete";
         newTagDelete.innerHTML = "[x]";
         newTagDelete.tag = tag;
         dojo.connect(newTagDelete, 'onclick', this, "_deleteTagEventHandler");
        
         newTagDiv.appendChild(newTag);
         newTagDiv.appendChild(newTagDelete);

         this.taglistui.insertBefore(newTagDiv, this.taglistui.firstChild);
         this.taglist[this.taglist.length] = tag;
       }
     },
     _deleteTagEventHandler: function(event)
     {
       var taglist = [event.target.tag];
       this._deleteTags(taglist);
     },
     _deleteTags: function(taglist)
     {
       var tagdata = {imgid : this.imgid, taglist : taglist};
       var widget = this;
       dojo.rawXhrPost({
             url: "/delete-img-tags",
             handleAs: "json-comment-filtered",
             postData: dojo.toJson(tagdata),
             timeout: 1000,
             load: function(response, ioargs)
             {
               var tagstodelete = widget.taglist.slice(0);

               dojo.forEach
                 (tagstodelete,
                  function(tag)
                  {
                    widget._deleteTagDOMNode(tag);
                  },
                  this);

               dojo.forEach
                 // (response.imgid,
                 (response.taglist,
                  function(tag)
                  {
                    widget._addTagDOMNode(tag);
                  },
                  this);
               
               widget.tagentrybox.setValue("");
             },
             error: function(response, ioargs)
             {
               alert(ioargs.xhr.status + ": " + response);
             }
       });
     },
     _doesTagExist: function(tag)
     {
       for (i = 0; i < this.taglist.length; ++i)
       {
         if (tag === this.taglist[i])
         {
           return 1;
         }
       }

       return 0;
     },
     _deleteTagDOMNode: function(tag)
     {
       var i = 0;
       var tagFound = 0;

       for (i = 0; i < this.taglist.length; ++i)
       {
         if (tag === this.taglist[i])
         {
           tagFound = 1;
           break;
         }
       }

       if (tagFound)
       {
         this.taglist.splice(i, 1);
       }

       for (i = 0; i < this.taglistui.childNodes.length; ++i)
       {
         if (this.taglistui.childNodes[i].tag === tag)
         {
           this.taglistui.removeChild(this.taglistui.childNodes[i]);
         }
       }
     }
 }
);
