var i = 0;
function addDuplicate(people, barColor) {
    var description = document.createElement('div');
    description.innerHTML = people[0].name + " & " + people[1].name + " could be the same person.";
    description.style.float='right';
    description.style.clear='right';
    var d = document.getElementById("main-panel");
    d.appendChild(description);
    var canvas = document.createElement('canvas');
    canvas.id = "canvas" + i;
    canvas.width=550;
    canvas.height=135;
    canvas.style.float='right';
    canvas.style.clear='right';
    canvas.style.paddingRight='10px';
    canvas.style.paddingBottom='20px';
    var d = document.getElementById("main-panel");
    d.appendChild(canvas);

    var margin = 50;
    var textMargin = 5;
    var textHeight = 10;
    var dupFont = textHeight + "pt Arial";
    var nextBlockY = textMargin;
    var blockHeight = 50;
    var spaceBetweenBlocks = 15;

    var context = canvas.getContext("2d");
    var width = $('#canvas'+i).attr('width').replace('px','');
    var height = $('#canvas'+i).attr('height').replace('px','');

    rect(context, 0,0,width,height,'#ffffff', true);
    var min = 9999;
    var max = 0;

    people.forEach(function (e) {
        if (typeof e.death != 'undefined' && e.death == 'Living') {
            max = new Date().getFullYear();
        }
    });

    people.forEach(function (e) {
        if (e.birth < min && e.birth!='?' && e.birth!='Living') min = e.birth;
        if (e.death < min && e.death!='?' && e.death!='Living') min = e.death;

        if (e.death > max && e.death!='?' && e.death!='Living') max = e.death;
        if (e.birth > max && e.birth!='?' && e.birth!='Living') max = e.birth;
    })


    people.forEach(function (e) {
        if (e.birth == max) max = parseInt(max)+1;
    });

    people.forEach(function (e) {
        var start = 0;
        var end = width - (margin*2);
        var birth = e.birth;
        var death = e.death;

        if (max-min==0) {
            max=1000;
            min-1;
        }
        if (typeof e.birth != 'undefined' && e.birth != "?" && e.death != "Living") {
            start = (e.birth-min) * (width - (margin*2)) / (max-min);
            birth = e.birth;
        }
        if (typeof e.death != 'undefined' && e.death != "?" && e.death != "Living") {
            end = (e.death-min) * (width - (margin*2)) / (max-min);
            death = e.death;
        }

        start += margin;
        end += margin;

        context.font = dupFont;
        var nameTransform = 0;
        var nameTextWidth = context.measureText(e.name).width;
        var textWidthA = context.measureText("Birth:").width
        var textWidthB = context.measureText(birth).width

        // Is the life bar shorter than the name?
        if ((end - start) < nameTextWidth) {
            // Move out the death text
            if (e.death != 'undefined' && e.death !='?')
                nameTransform = nameTextWidth - (end - start) + textMargin;
            // Are we going out of bounds?
            if (end + nameTextWidth + textWidthB >  width) {
                start -= ((end+nameTextWidth + textWidthB + textMargin*5) - width);
                //end -= ((end+nameTextWidth + textWidthB + textMargin*5) - width);
            }
        }

        var color1 = barColor;
        var color2 = barColor;
        var color3 = barColor;

        var grd=context.createLinearGradient(start,0,end,0);

        if (e.birth != 'undefined' && e.birth == '?')
            color1 = 'white';
        if (e.death != 'undefined' && e.death == '?')
            color3 = 'white';

        grd.addColorStop(0,color1);
        grd.addColorStop(0.5,color2);
        grd.addColorStop(1,color3);

        rect(context, start, nextBlockY, end - start, blockHeight, grd, true);
        text(context, start - textWidthA - textMargin, nextBlockY + textHeight+ textMargin, "#000000", "Birth:");
        text(context, start - textWidthB - textMargin, nextBlockY + textHeight*2+ textMargin*2, "#000000", birth);

        text(context, end + textMargin+nameTransform, nextBlockY+ textHeight+ textMargin, "#000000", "Death:");
        text(context, end + textMargin+nameTransform, nextBlockY+ textHeight*2+ textMargin*2, "#000000", death);
        text(context, start + textMargin, nextBlockY + textHeight + textMargin, "#000000", e.name);
        nextBlockY += (blockHeight + spaceBetweenBlocks);
    });

    i++;
}

function text(context, x, y, color, text, textSize) {
    context.fillStyle = color;
    context.fillText(text, x, y);
}

function rect(context, x, y, height, width, color, border) {
    context.beginPath();
    context.rect(x, y, height, width);

    context.fillStyle=color;
    context.fill();
    if (border) {
        context.strokeStyle = 'black';
        context.stroke();
    }

}
