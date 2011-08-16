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
            display(ctx, picture, 1, 0, 0, 1, 0, 0);
        }

        function display(ctx, pic, a, b, c, d, e, f)
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
                    display(ctx, pic[i], a, b, c, d, e, f);
                }
            }
            else if (pic.t == "c")
            {
                ctx.save();
                ctx.transform(a, b, c, d, e, f);
                ctx.beginPath();
                ctx.arc(0, 0, pic.r, 0, 2 * Math.PI);
                ctx.restore();
                ctx.stroke();
            }
            else if (pic.t == "p")
            {
                if (pic.p.length > 0)
                {
                    ctx.save();
                    ctx.transform(a, b, c, d, e, f);
                    ctx.beginPath();
                    ctx.moveTo(pic.p[0].x, pic.p[0].y);
                    var i;
                    for (i = 1; i < pic.p.length; i++)
                    {
                        ctx.lineTo(pic.p[i].x, pic.p[i].y);
                    }
                    ctx.restore();
                    ctx.fill();
                }
            }
            else if (pic.t == "l")
            {
                if (pic.p.length > 0)
                {
                    ctx.save();
                    ctx.transform(a, b, c, d, e, f);
                    ctx.beginPath();
                    ctx.moveTo(pic.p[0].x, pic.p[0].y);
                    var i;
                    for (i = 1; i < pic.p.length; i++)
                    {
                        ctx.lineTo(pic.p[i].x, pic.p[i].y);
                    }
                    ctx.restore();
                    ctx.stroke();
                }
            }
            else if (pic.t == 'h')
            {
                ctx.save();
                ctx.save();
                ctx.transform(a, b, c, d, e, f);
                ctx.beginPath();
                ctx.arc(0, 0, pic.r, 0, 2 * Math.PI);
                ctx.restore();
                ctx.lineWidth = pic.w;
                ctx.stroke();
                ctx.restore();
            }
            else if (pic.t == 't')
            {
                ctx.save();
                ctx.transform(a, b, c, d, e, f);
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
                display(ctx, pic.p, a, b, c, d, e, f);
                ctx.restore();
            }
            else if (pic.t == 'x')
            {
                display(ctx, pic.p,
                    a, b, c, d,
                    a * pic.x + c * pic.y + e,
                    b * pic.x + d * pic.y + f);
            }
            else if (pic.t == 'r')
            {
                var th = Math.PI * pic.r / 180;
                display(ctx, pic.p,
                     a * Math.cos(th) + c * Math.sin(th),
                     b * Math.cos(th) + d * Math.sin(th),
                    -a * Math.sin(th) + c * Math.cos(th),
                    -b * Math.sin(th) + d * Math.cos(th),
                    e, f);
            }
            else if (pic.t == 's')
            {
                display(ctx, pic.p,
                    pic.x * a, pic.x * b,
                    pic.y * c, pic.y * d,
                    e, f);
            }
            else if (pic.t == 'b')
            {
                ctx.save();
                ctx.transform(a, b, c, d, e, f);
                var img = new Image(pic.c);
                ctx.drawImage(img,
                    -img.naturalWidth / 2, -pic.naturalHeight / 2,
                    pic.naturalWidth, pic.naturalHeight);
                ctx.restore();
            }
        }
        </script>
        <displayScript/>
    </head>
    <body onload="displayInCanvas();">
        <canvas id="screen" width="500" height="500" style="border:solid black 1px"></canvas>
    </body>
</html>

