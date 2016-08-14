var defDate=undefined;
var old;
var cached={};
var img=0;
var cur=imgs[img];
var lastMkdir="";

function click(li)
{
		var t = $(li.currentTarget);
		var s = t.children().first();
		if(cur.dir)
				old = cur.dir.obj;

		cur.dir={'id' : t[0].id, 'name': s.text(), 'obj' : t};
		setImg(1);
		s.effect("highlight",{color:"#97FE5A"}, 250 );
		return false;
}

function wrap(diff){
		var img2 = (img + diff) % imgs.length;
		if(img2<0) img2 += imgs.length;
		// console.log([diff,img2]);
		return img2;
}

function setImg(diff){
		if(old)
				old.removeClass('select');

		img = wrap(diff);

		cur = imgs[img];
		
		if(cur.dir)
				old = cur.dir.obj;

		$('#large').empty();
		$('#large').append(preload(0));
		$('#largeSpan div').html(cur.id);
		if(cur.date){
				$('#largeSpan h4').html("Date: "+cur.date);
		}else
				$('#largeSpan h4').html("Date: &nbsp;");

		if(cur.dir){
				$('#dirs h4').html("Folder <span onclick='clearFolder()'>(clear)</span>: "+cur.dir.id);
				cur.dir.obj.addClass('select');
		}else
				$('#dirs h4').html("Folder: &nbsp;");
		dateDef();
		for(var i=0;i<10;i++)
				window.setTimeout(function(i){
						preload(i+1);
						preload(-i-1);},100+200*i,i);
}
function rot(angle){
  if(angle != 0)
    cur.angle=angle
	setImg(1);
}

function clearFolder(){
		if(cur.dir){
				old = cur.dir.obj;
				delete cur.dir;
				setImg(0);
		}
}

function imgUrl(scaledown,d){
		return "img?id="+(d?d:cur.id)+
				(scaledown?"&size="+((screen.width)/2)
				 + "x"
				 //+(screen.height - 150)
				: "");
}

function init(){
	$("li").on('click',click);
	$("li").mouseover(function(e){$(e.currentTarget).addClass('hover');return false;})
			.mouseout(function(e){$(e.currentTarget).removeClass('hover');return false;});
}

$(function() {
	if(imgs.length > 0)
			setImg(0);
	$('#large').on('click',function(){window.open(imgUrl(),'_blank');});
  init();
});

document.onkeydown = function(event) {
    if (!event)
        event = window.event;
    var code = event.keyCode;
    if (event.charCode && code == 0)
        code = event.charCode;
		console.log("shift: "+event.shiftKey);
		console.log("ctrl: "+event.ctrlKey);
		console.log(code);
		var i=$("#input");
		var n=undefined;
		if((code < 58 && code > 47) || code == 32) //number || space
				n=String.fromCharCode(code);
		if(code < 106 && code > 95) //numpad
				n=String.fromCharCode(code-48);
		if(n){
				if(defDate)
				{
						defDate=false;
						i.html(n);
				}else
						i.html(i.text()+n);
				return false;
		}
    switch(code) {
    case 37://left
        if(event.ctrlKey){
          rot(90);
          return false;
        }
				if(event.shiftKey)
						swap(-1);
				setImg(-1);
        return false;
    case 38://up
        if(event.ctrlKey){
          rot(0);
          return false;
        }
        break;
    case 39://right
        if(event.ctrlKey){
          rot(-90);
          return false;
        }
				if(event.shiftKey)
						swap(1);
				setImg(1);
        return false;
    case 36://home
				setImg(-img);
        return false;
    case 35://end
				setImg(imgs.length-1-img);
        return false;
    case 46://Del
				delete cur.date;
				setImg(0);
				return false;
    case 8://Backspace
				dateDef();
				return false;
      case 77://m
        var name=prompt("Enter dir name",lastMkdir);
        if(name)
          lastMkdir = name;
		      $.get('mkdir',{ 'name' : name },function(data){
            $('#dirs ul').remove();
            $('#dirs h4').after(data);
            init();
          });
				return false;
    case 40://down
        if(event.ctrlKey){
          rot(180);
          return false;
        }
        break;
    case 68://'d'
				defDate=false;
				i.html("delete");
    case 13://enter
				var b=i.text().trim().split(" ");
				var da=i.text();
				switch(b.length){
				case 2:
						da=[b[0],b[1],16].join(".");
						break;
				case 3:
						da=[b[0],b[1],b[2]].join(".");
						break;
				default:
						if(!defDate && da!="delete")
								return;
				}
				cur.date=da;
				setImg(1);
        break;
		default:
    }
    // event.preventDefault();
};

function swap(){
}

function dateDef()
{
		defDate=true;
		var d=imgs[img-1];
		var nd="&nbsp;";
		if(d && d.date && d.date!="delete")
				nd = d.date;
		$("#input").html(nd);
}

function preload(o){
		var i=imgs[wrap(o)];
		if(!i) return;
		i=i.id;
		if(!cached[i]){
				cached[i] = new Image();
				if(1){
						cached[i].src=imgUrl(1,i);
				}else{
						cached[i].src=imgUrl(0,i);
						cached[i].height=screen.height - 100;
				}
		// $(l).css({'position':'absolute'
		// 				 });// .hide().css(;
				// $("#imgSpan").append(l);
		}
		// console.log(cached);
		return cached[i];
}


function swap(i){
		imgs[img]=imgs[img+i];
		imgs[img+i]=cur;
}

function applyChanges(){
		// console.log(imgs);
		$.post('apply',{ 'imgs' : JSON.stringify(imgs)},function(data){
				$('body').empty().append($('<pre>').html(data));
				$('body').prepend("<a href='?'>reload page<a/>");
		});
		return false;
}
