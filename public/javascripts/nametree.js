var context;
var canvas;
var taken = [];
function drawTree(words) {
    var colors = ["#7D8A2E",
        "#C9D787",
        "#7E8AA2",
        "#D8CAA8",
        "#284907",
        "#382513",
        "#468966",
        "#5C832F",
        "#FFB03B",
        "#363942",
        "#B64926",
        "#8E2800",
        "#263248",
        "#FF9800",
        "#FFC0A9",
        "#69D2E7",
        "#A7DBD8",
        "#E0E4CC",
        "#F38630",
        "#FA6900",
        "#FC9D9A",
        "#F9CDAD",
        "#C8C8A9",
        "#83AF9B",
        "#556270",
        "#C7F464",
        'CornflowerBlue', 'Crimson', 'DarkOrange', 'DarkOrchid', 'DarkTurquoise', 'Fuchsia', 'Gold', 'HotPink', 'LightSeaGreen', 'SteelBlue'];
//    var words = [
//        {name: 'David', size: 60},
//        {name: 'Liz', size: 40},
//        {name: 'Grace', size: 35},
//
//// {name:'Liz',size:30},
//// {name:'Cameron',size:30},
//// {name:'Liz',size:30},
////   {name:'Grace',size:30},
//
//        {name: 'Tom', size: 10},
//        {name: 'Jennie', size: 10},
//        {name: 'Justin', size: 10},
//        {name: 'Tom', size: 10},
//        {name: 'Jennie', size: 10},
//        {name: 'Justin', size: 10},
//        {name: 'Tom', size: 10},
//        {name: 'Jennie', size: 10},
//        {name: 'Justin', size: 10},
//        {name: 'Doug', size: 10},
//        {name: 'Ben', size: 10},
//        {name: 'Jordan', size: 10},
//        {name: 'John', size: 10},
//        {name: 'Doug', size: 10},
//        {name: 'Ben', size: 10},
//        {name: 'Jordan', size: 10},
//        {name: 'John', size: 10},
//        {name: 'David', size: 10},
//        {name: 'Cameron', size: 10},
//        {name: 'Liz', size: 10},
//        {name: 'Grace', size: 10},
//        {name: 'Emma', size: 10},
//        {name: 'Nick', size: 10},
//        {name: 'Tom', size: 10},
//        {name: 'Liz', size: 30},
//        {name: 'Cameron', size: 30},
//        {name: 'Liz', size: 30},
//        {name: 'Grace', size: 30},
//
//        {name: 'Jennie', size: 10},
//        {name: 'David', size: 10},
//        {name: 'Cameron', size: 10},
//        {name: 'Liz', size: 10},
//        {name: 'Grace', size: 10},
//        {name: 'David', size: 10},
//        {name: 'Cameron', size: 10},
//        {name: 'Liz', size: 10},
//        {name: 'Grace', size: 10},
//        {name: 'Emma', size: 10},
//        {name: 'Nick', size: 10},
//        {name: 'Tom', size: 10},
//        {name: 'Jennie', size: 10},
//        {name: 'David', size: 10},
//        {name: 'Cameron', size: 10},
//        {name: 'Liz', size: 10},
//        {name: 'Grace', size: 10},
//        {name: 'Emma', size: 25},
//        {name: 'Nick', size: 25},
//        {name: 'Emma', size: 25},
//        {name: 'Nick', size: 25},
//        {name: 'Tom', size: 10},
//        {name: 'Jennie', size: 10},
//        {name: 'Justin', size: 10},
//        {name: 'Tom', size: 10},
//        {name: 'Jennie', size: 10},
//        {name: 'Justin', size: 10},
//        {name: 'Tom', size: 10},
//        {name: 'Jennie', size: 10},
//        {name: 'Justin', size: 10},
//        {name: 'Doug', size: 10},
//        {name: 'Ben', size: 10},
//        {name: 'Jordan', size: 10},
//        {name: 'John', size: 10},
//        {name: 'Doug', size: 10},
//        {name: 'Ben', size: 10},
//        {name: 'Jordan', size: 10},
//        {name: 'John', size: 10},
//        {name: 'David', size: 10},
//        {name: 'Cameron', size: 10},
//        {name: 'Liz', size: 10},
//        {name: 'Grace', size: 10},
//        {name: 'Emma', size: 10},
//        {name: 'Nick', size: 10},
//        {name: 'Tom', size: 10},
//        {name: 'Jennie', size: 10},
//        {name: 'David', size: 10},
//        {name: 'Cameron', size: 10},
//        {name: 'Liz', size: 10},
//        {name: 'Grace', size: 10},
//        {name: 'David', size: 10},
//        {name: 'Cameron', size: 10},
//        {name: 'Liz', size: 10},
//        {name: 'Grace', size: 10},
//        {name: 'Emma', size: 10},
//        {name: 'Nick', size: 10},
//        {name: 'Tom', size: 10},
//        {name: 'Jennie', size: 10},
//        {name: 'David', size: 10},
//        {name: 'Cameron', size: 10},
//        {name: 'Liz', size: 10},
//        {name: 'Emma', size: 10}
//
//    ];
    canvas = document.getElementById('canvas');
    context = canvas.getContext('2d');

    var rr = 500;
    var sz = 40;
    context.font = sz + 'pt Calibri';
    //   context.fillText('asdfasdf', 0,10);
    //   var context=c.getContext("2d");
    context.fillStyle = "#ffffff";
    context.strokeStyle = "#e0e0e0";
    context.lineWidth = 3;
    context.lineCap = "round";
    context.lineJoin = "round";
    context.beginPath();
    context.moveTo(168.37659,427);
    context.lineTo(198.92743,380.64658);
    context.lineTo(208.22457999999997,357.46939);
    context.lineTo(209.55337999999998,335.65587999999997);
    context.lineTo(206.89666999999997,305.66234999999995);
    context.lineTo(197.59861999999998,292.02876999999995);
    context.lineTo(180.33132999999998,283.84880999999996);
    context.lineTo(161.73525999999998,283.84880999999996);
    context.lineTo(144.46796999999998,296.11877);
    context.bezierCurveTo(139.15454999999997,300.66326,136.94106999999997,305.22936999999996,128.52858999999998,309.75235);
    context.bezierCurveTo(120.11611999999998,314.27529999999996,111.70452999999998,317.90085,103.29205999999998,317.93228);
    context.lineTo(80.71125999999998,316.56955);
    context.lineTo(56.802379999999985,308.38873);
    context.lineTo(34.22184999999999,293.39239000000003);
    context.lineTo(18.28264999999999,272.94250000000005);
    context.lineTo(10.31304999999999,255.21893000000006);
    context.lineTo(4.99997999999999,233.40539000000007);
    context.lineTo(4.99997999999999,212.95547000000008);
    context.lineTo(7.65650999999999,189.7783000000001);
    context.lineTo(15.62611999999999,173.4183600000001);
    context.lineTo(27.58051999999999,154.3311700000001);
    context.lineTo(42.19144999999999,137.9712500000001);
    context.lineTo(47.052739999999986,113.43134000000012);
    context.lineTo(50.16104999999999,94.34415000000013);
    context.lineTo(58.13064999999999,76.62059000000013);
    context.lineTo(67.42859999999999,67.07717000000014);
    context.lineTo(82.03953999999999,56.17041000000014);
    context.lineTo(96.65072999999998,45.26363000000014);
    context.lineTo(112.58921999999998,38.44694000000014);
    context.lineTo(128.52858999999998,34.35696000000014);
    context.lineTo(143.14005999999998,35.72032000000014);
    context.lineTo(159.07943999999998,39.81030000000014);
    context.lineTo(171.03329999999997,43.90037000000014);
    context.lineTo(185.64476999999997,32.993600000000136);
    context.lineTo(198.92743999999996,20.723470000000134);
    context.lineTo(217.52261999999996,9.816700000000134);
    context.lineTo(237.44662999999997,3.0000200000001342);
    context.lineTo(257.37060999999994,3.0000200000001342);
    context.lineTo(274.63787999999994,8.453400000000133);
    context.lineTo(290.57726999999994,17.996760000000133);
    context.lineTo(301.20321999999993,26.176910000000134);
    context.lineTo(313.15795999999995,37.083590000000136);
    context.lineTo(318.47140999999993,50.717070000000135);
    context.lineTo(335.7387099999999,53.443790000000135);
    context.lineTo(351.6780399999999,58.89713000000013);
    context.lineTo(363.6319299999999,67.07718000000013);
    context.lineTo(372.9299599999999,76.62060000000012);
    context.lineTo(380.90010999999987,87.52737000000012);
    context.lineTo(386.21260999999987,103.88775000000012);
    context.lineTo(387.54143999999985,114.79408000000012);
    context.lineTo(402.15200999999985,120.24769000000012);
    context.lineTo(414.10674999999986,129.7912800000001);
    context.lineTo(423.40389999999985,142.0612200000001);
    context.lineTo(430.04525999999987,154.3311800000001);
    context.lineTo(435.3586799999999,167.9647600000001);
    context.lineTo(442.00000999999986,188.4146800000001);
    context.lineTo(441.10999999999984,202.04826000000008);
    context.lineTo(435.35868999999985,217.04545000000007);
    context.lineTo(424.2939299999999,231.57644000000008);
    context.lineTo(408.79334999999986,248.40170000000006);
    context.lineTo(398.61863999999986,259.2765600000001);
    context.lineTo(385.7747699999999,268.8201700000001);
    context.lineTo(370.27326999999985,275.66886000000005);
    context.bezierCurveTo(364.0751799999999,277.9307600000001,357.87613999999985,280.19181000000003,351.67805999999985,279.75883000000005);
    context.lineTo(335.73872999999986,277.03248);
    context.lineTo(318.4714299999999,277.03248);
    context.lineTo(297.2186499999999,278.39518000000004);
    context.lineTo(285.2647599999999,289.30241000000007);
    context.lineTo(275.9667299999999,305.6623600000001);
    context.lineTo(275.9667299999999,324.7495500000001);
    context.lineTo(279.95134999999993,339.7458600000001);
    context.lineTo(283.93596999999994,350.6530600000001);
    context.lineTo(288.78392999999994,368.3766300000001);
    context.lineTo(298.54741999999993,388.8265500000001);
    context.lineTo(317.1425999999999,413.3664700000001);
    context.lineTo(327.76857999999993,427.0000100000001);
    context.lineTo(248.07260999999994,427.0000100000001);
    context.lineTo(168.37660999999994,427.0000100000001);
    context.stroke();
    //    context.fillText('asdfasdf', -60+rr,-60+rr);
    var g = 0;
    for (u = 0; u < 1; u++) {
        console.log(u);
        words.forEach(function (w) {
            console.log(g + '/' + words.length);
            g++;
            context.fillStyle = colors[(Math.floor(Math.random() * 100 % colors.length))];
            if (w.size > 10) w.size = w.size * 1.5
            placeText(w.name.toUpperCase(), w.size);
        });
        // for(y=0;y<200;y++){
        // console.log(g+'/'+words.length);
        // g++;
        //   context.fillStyle=colors[(Math.floor(Math.random() * 100 % colors.length))];
        //   //if (w.size > 10) w.size = w.size * 1.5
        //   placeText('.', Math.random() * 100 % 40 + 10);
        // }
    }

};

function placeText(text, sz) {
    context.font = sz + 'pt Calibri';
    var cwidth = context.measureText(text).width;
    var cheight = sz;//context.measureText(text).height;
    var x = 100;
    var y = 100;
    var canput = false;
    var xbounds = 5;
    var ybounds = 5;
    var xdelta = 0;
    var ydelta = 0;
    var inc = 5;
    var centerCoord = 200;
    do {
        canput = true;
        //  (xdelta+','+ydelta+' .. '+xbounds+','+ybounds);
//  console.log('start');
        taken.forEach(function (item) {
            if (!testPut(xdelta, ydelta, cwidth, cheight, item.x, item.y, item.width, item.height, text))
                canput = false;

        });
        //  console.log('end');
        //  var d = document.getElementById('d');
        //  $('body').append("<div  style='background-color:#ff0000;width:10px;height:10px;position:absolute;left:"+xdelta+"px;top:"+ydelta+"px'></div>");

//d.style.position = "absolute";
//d.style.left = (xdelta)+'px';
//d.style.top = (ydelta)+'px';
        if (xdelta < xbounds && inc > 0) {
            xdelta += inc;
        }
        else if (ydelta < ybounds && inc > 0) {
            ydelta += inc;
        }
        else if (xdelta > xbounds && inc < 0) {
            xdelta += inc;
        }
        else if (ydelta > ybounds && inc < 0) {
            ydelta += inc;
        } else if (xdelta == xbounds && ydelta == ybounds) {
            inc = -inc
            xbounds = -xbounds
            ybounds = -ybounds
            if (inc < 0) {
                xbounds += inc;
                ybounds += inc;
// if (Math.floor(Math.random()*100)%2==0)
//   xbounds += inc;
// if (Math.floor(Math.random()*100)%2==0)
//   ybounds += inc;
            }
        }
        else {
            xbounds += inc;
            ybounds += inc;
        }
        var precalcxdelta = xdelta + centerCoord;
        var precalcydelta = ydelta + centerCoord;
    } while (xdelta + centerCoord > -100 && (canput == false || !context.isPointInPath(precalcxdelta + cwidth, precalcydelta) || !context.isPointInPath(precalcxdelta, precalcydelta) || !context.isPointInPath(precalcxdelta + cwidth, precalcydelta - cheight) || !context.isPointInPath(precalcxdelta, precalcydelta - cheight)
        ));
    if (xdelta + 200 > 0) {
        taken.push({x: xdelta, y: ydelta, width: cwidth, height: cheight, txt: text});
        context.fillText(text, precalcxdelta, precalcydelta);
    }
}

//change this.. the x1,y1 is the bottom left corner of a lower case a
function testPut(x1, y1, width1, height1, x2, y2, width2, height2, text) {
    if (x1 + width1 < x2 || // To the right of
        y1 < y2 - height2 || // Under
        x1 > x2 + width2 || // To the left of
        y1 - height1 > y2) // Above
        return true;
    return false;
}
