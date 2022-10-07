<?php include "top.html"; ?>
    <div>
        <form method="get" action="matches-submit.php">
            <fieldset>
                <legend>Returning User:</legend>
                <ul>
                    <li>
                        <label class="column left" ><strong>Name</strong></label>
                        <input name="name" type="text" size="20" maxlength="16">
                    </li>
                </ul>

                <input type="submit" value="View My Matches"">
            </fieldset>
        </form>
    </div>
<?php include "bottom.html"; ?>