function GetImgTitle(imgid, setterfn)
{
  var get_title_request_json = {imgid : imgid};
  dojo.rawXhrPost({
        url: "/get-img-title",
        handleAs: "json-comment-filtered",
        postData: dojo.toJson(get_title_request_json),
        timeout: 1000,
        load: function(response, ioargs)
        {
          setterfn(response.title);
        },
        error: function(response, ioargs)
        {
          alert(ioargs.xhr.status + ": " + response);
        }
  });
}

function SetImgTitle(imgid, title, setterfn)
{
  var set_title_request_json = {imgid : imgid, title: title};
  dojo.rawXhrPost({
        url: "/set-img-title",
        handleAs: "json-comment-filtered",
        postData: dojo.toJson(set_title_request_json),
        timeout: 1000,
        load: function(response, ioargs)
        {
          setterfn(response.title);
        },
        error: function(response, ioargs)
        {
          alert(ioargs.xhr.status + ": " + response);
        }
  });
}

function GetImgDescription(imgid, setterfn)
{
  var get_description_request_json = {imgid : imgid};
  dojo.rawXhrPost({
        url: "/get-img-description",
        handleAs: "json-comment-filtered",
        postData: dojo.toJson(get_description_request_json),
        timeout: 1000,
        load: function(response, ioargs)
        {
          setterfn(response.description);
        },
        error: function(response, ioargs)
        {
          alert(ioargs.xhr.status + ": " + response);
        }
  });
}

function SetImgDescription(imgid, description, setterfn)
{
  var set_description_request_json = {imgid : imgid, description: description};
  dojo.rawXhrPost({
        url: "/set-img-description",
        handleAs: "json-comment-filtered",
        postData: dojo.toJson(set_description_request_json),
        timeout: 1000,
        load: function(response, ioargs)
        {
          setterfn(response.description);
        },
        error: function(response, ioargs)
        {
          alert(ioargs.xhr.status + ": " + response);
        }
  });
}
