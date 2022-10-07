<?php include "top.html"; ?>
<div>
    <form method="post" action="signup-submit.php">
        <fieldset>
            <legend>New User Signup:</legend>
            <ul>
            <li>
                <label class="column left" ><strong>Name</strong></label>
                <input name="name" type="text" size="20" maxlength="16">
            </li>
            <li>
                <label class="column left"><strong>Gender:</strong></label>
                <input type="radio" name="gender" value="M"><label>Male</label>
                <input type="radio" name="gender" value="F" checked><label>Female</label>
            </li>
            <li>
                <label class="column left"><strong>Age</strong></label>
                <input name="age" type="text" size="6" maxlength="2">
            </li>
            <li>
                <label class="column left" ><strong>Personality type</strong></label>
                <input name="personality" type="text" size="6" maxlength="4">(<a href="http://www.humanmetrics.com/cgi-win/jtypes2.asp">Don't know your type?</a>)
            </li>
            <li>
                <label class="column left" ><strong>Favourite OS:</strong></label>
                <select name="OS">
                    <option value="Linux">Linux</option>
                    <option value="Windows">Winzoz</option>
                    <option value="mac">Mac OS X</option>
                </select>
            </li>
            <li>
                <label class="column left" ><strong>Seeking age:</strong></label>
                <input name="minAge" type="text" size="6" maxlength="2" placeholder="min"> to <input name="maxAge" type="text" size="6" maxlength="2" placeholder="max">
            </li>
            </ul>
            <input type="submit" value="Sign Up">
        </fieldset>
    </form>
</div>


<?php include "bottom.html"; ?>