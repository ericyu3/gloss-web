<html>
    <head>
        <!--[if IE]>
        <script type="text/javascript" src="excanvas.js"></script>
        <![endif]-->

        <script type="text/javascript">
        function displayInCanvas()
        {
            var c = document.getElementById("screen");
            var ctx = c.getContext("2d");
            ctx.translate(250, 250);
            ctx.scale(1, -1);
            ctx.textAlign = "left";
            ctx.textBaseline = "alphabetic";
            ctx.lineWidth = 0;
            ctx.font = "100px Times Roman";
            display(ctx, picture);
        }

        function display(ctx, pic)
        {
            if (pic == null)
            {
                // do nothing
            }
            else if (pic instanceof Array)
            {
                var i;
                for (i = 0; i < pic.length; i++)
                {
                    display(ctx, pic[i]);
                }
            }
            else if (pic.t == "c")
            {
                ctx.beginPath();
                ctx.arc(0, 0, pic.r, 0, 2 * Math.PI);
                ctx.stroke();
            }
            else if (pic.t == "p")
            {
                if (pic.p.length > 0)
                {
                    ctx.beginPath();
                    ctx.moveTo(pic.p[0].x, pic.p[0].y);
                    var i;
                    for (i = 1; i < pic.p.length; i++)
                    {
                        ctx.lineTo(pic.p[i].x, pic.p[i].y);
                    }
                    ctx.fill();
                }
            }
            else if (pic.t == "l")
            {
                if (pic.p.length > 0)
                {
                    ctx.beginPath();
                    ctx.moveTo(pic.p[0].x, pic.p[0].y);
                    var i;
                    for (i = 1; i < pic.p.length; i++)
                    {
                        ctx.lineTo(pic.p[i].x, pic.p[i].y);
                    }
                    ctx.stroke();
                }
            }
            else if (pic.t == 'h')
            {
                ctx.save();
                ctx.lineWidth = pic.w;
                ctx.beginPath();
                ctx.arc(0, 0, pic.r, 0, 2 * Math.PI);
                ctx.stroke();
                ctx.restore();
            }
            else if (pic.t == 't')
            {
                ctx.save();
                ctx.scale(1,-1);
                ctx.fillText(pic.c, 0, 0);
                ctx.restore();
            }
            else if (pic.t == 'z')
            {
                var str = "rgba("
                    + pic.r + "," + pic.g + "," + pic.b + "," + pic.a + ")";
                ctx.save();
                ctx.strokeStyle = str;
                ctx.fillStyle = str;
                display(ctx, pic.p);
                ctx.restore();
            }
            else if (pic.t == 'x')
            {
                ctx.save();
                ctx.translate(pic.x, pic.y);
                display(ctx, pic.p);
                ctx.restore();
            }
            else if (pic.t == 'r')
            {
                ctx.save();
                ctx.rotate(Math.PI * pic.r / 180);
                display(ctx, pic.p);
                ctx.restore();
            }
            else if (pic.t == 's')
            {
                ctx.save();
                ctx.scale(pic.x, pic.y);
                display(ctx, pic.p);
                ctx.restore();
            }
            else if (pic.t == 'b')
            {
                var img = new Image(pic.c);
                alert(img.complete + " " + img.naturalWidth + " " + img.naturalHeight); 
                ctx.drawImage(img,
                    -img.naturalWidth / 2, -pic.naturalHeight / 2,
                    pic.naturalWidth, pic.naturalHeight);
            }
        }
        </script>
        <displayScript/>
    </head>
    <body onload="displayInCanvas();">
        <canvas id="screen" width="500" height="500" style="border:solid black 1px"></canvas>
    </body>
</html>

